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


object TestRefMany {
    
  case class Foo(id:Int) extends HasId[Int]
  
  case class Bar(id:Int) extends HasId[Int]  
  
  implicit object gg extends GetsId[HasId[Int], Int] {
    def getId(obj: HasId[Int]) = Some(obj.id)

    def canonical(key: Any) = key match {
      case k: Int => Some(k)
      case _ => None
    }    
  }
  
 
  
  implicit object LookupBar extends LookUp[Bar, Int] {
    def lookUpMany(r:RefManyById[Bar, Int]) = (for { id <- r.rawIds } yield Bar(id)).toRefMany
    
    def lookUpOne(r:RefById[Bar, Int]) = Bar(r.id).itself
  } 
  

  implicit object LookupFoo extends LookUp[Foo, Int] {
    def lookUpMany(r:RefManyById[Foo, Int]) = (for { id <- r.rawIds } yield Foo(id)).toRefMany
    
    def lookUpOne(r:RefById[Foo, Int]) = Foo(r.id).itself
  } 
}

class TestRefMany {

  import TestRefMany._
  
  @Test
  def simpleRefMany {    
    val seq2 = Stream(Foo(1), Foo(2), Foo(3), Foo(4))    
    val str = Stream(1, 2, 3, 4)
    
    val rm = new RefManyById(classOf[Foo], str)        
    assertEquals(seq2.toSeq, rm.fetch.toSeq)    
  }  
  
}