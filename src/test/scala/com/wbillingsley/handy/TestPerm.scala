package com.wbillingsley.handy

import org.junit.{BeforeClass, Test}
import org.junit.Assert._
import scala.collection.mutable

import scala.language.{implicitConversions, postfixOps}
import Ref._

import scala.concurrent._
import ExecutionContext.Implicits.global
import Future._
import Ref._
import RefMany._



object TestPerm {
  
  case class Foo(id:Int) extends HasId[Int]
  
  case class Bar(id:Int) extends HasId[Int]
  
  
  implicit object gg extends GetsId[HasId[Int], Int] {
    def getId(obj: HasId[Int]) = Some(obj.id)

    def canonical(key: Any) = key match {
      case k: Int => Some(k)
      case _ => None
    }    
  }
  
  case class FooPerm(r:Ref[Foo]) extends PermOnIdRef(r) {
    def resolve(prior:Approval[Nothing]) = Approved("yes")
  }

  case class BarPerm(r:Ref[Bar]) extends PermOnIdRef(r) {
    def resolve(prior:Approval[Nothing]) = Approved("yes")
  }

}

import TestPerm._

class TestPerm {
  
  @Test def permOnIdEquals {
    
    val foo1 = Foo(1)
    val foo2 = Foo(2)
    
    assertEquals(true, FooPerm(foo1.itself) == FooPerm(RefById(classOf[Foo], 1)))
    assertEquals(true, FooPerm(RefById(classOf[Foo], 1)) == FooPerm(foo1.itself) )
    
    
    assertEquals(false, FooPerm(foo1.itself) == FooPerm(RefById(classOf[Foo], 2)))
    assertEquals(false, FooPerm(RefById(classOf[Foo], 2)) == FooPerm(foo1.itself))
    
    assertEquals(false, FooPerm(foo1.itself) == BarPerm(RefById(classOf[Bar], 1)))
         
  }
  
}