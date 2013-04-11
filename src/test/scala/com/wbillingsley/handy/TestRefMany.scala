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
  
  @BeforeClass
  def beforeClass = {
        
    val m:Map[Class[_], Function1[Int, _]] = Map(classOf[Foo] -> ((i:Int) => Foo(i)), classOf[Bar] -> ((i:Int) => Bar(i)))  
    
    RefManyById.lookUpMethod = new RefManyById.LookUp {      
      def lookup[T](rm:RefManyById[T, _]) = {
        val s = rm.rawIds
        
        val maker = m.get(rm.clazz)
        maker match {
          case Some(f) => s.map((i) => f(i.toString.toInt).asInstanceOf[T]).toRefMany
          case None => RefFailed(new IllegalArgumentException("Don't know how to look that up"))
        }
      }
    }
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