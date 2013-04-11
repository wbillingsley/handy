package com.wbillingsley.handyplay;

import play.api.libs.json._
import play.api.libs.iteratee.{Iteratee, Enumerator, Enumeratee}
import play.api.test.WithApplication
import org.specs2.mutable._
import scala.concurrent.ExecutionContext.Implicits.global

import com.wbillingsley.handy.{ RefFuture, RefFutureOption }
import com.wbillingsley.handy.Ref._
import RefConversions._
import EnumeratorHelper._

class RefEnumeratorSpec extends Specification {
  
  sequential
  
  "Helper" should {
    
    "correctly identify a sequence" in {      
      val items = Enumerator(1, 2, 3) andThen Enumerator.eof
      items.verify(List(_ == 1, _ == 2, _ == 3))      
    }
    
    "complain about incorrect items in a sequence" in {      
      val items = Enumerator(1, 2, 3) andThen Enumerator.eof
      items.verify(List(_ == 1, _ == 4, _ == 3)) must throwA[RuntimeException]("Element failed check: 2")     
    }
    
    "complain about too may items in a sequence" in {      
      val items = Enumerator(1, 2, 3, 4) andThen Enumerator.eof
      items.verify(List(_ == 1, _ == 2, _ == 3)) must throwA[RuntimeException]("Element received after checks exhausted: 4")
    }
    
    "complain about a premature EOF" in {      
      val items = Enumerator(1, 2) andThen Enumerator.eof andThen Enumerator(3) andThen Enumerator.eof
      items.verify(List(_ == 1, _ == 2, _ == 3)) must throwA[RuntimeException]("EOF received before checks were exhausted. Remaining: 1")
    }       
    
    "complain about too few items in a sequence" in {      
      val items = Enumerator(1, 2) 
      items.verify(List(_ == 1, _ == 2, _ == 3)) must throwA[RuntimeException]("EOF received before checks were exhausted. Remaining: 1")
    }
  }
  
  
  "RefEnumerator" should {
    
    "enumerate each item" in {
      
      val items = Enumerator(1, 2, 3, 4)
      val re = new RefEnumerator(items)
      
      val output = for (num <- re) yield "Number " + num.toString
      
      val e = output.enumerate
      e.verify(List(
        _ == "Number 1",    
        _ == "Number 2",    
        _ == "Number 3",    
        _ == "Number 4"    
      ))
      
    }
    
    "filter successfully" in {
      
      val items = Enumerator(1, 2, 3, 4)
      val re = new RefEnumerator(items)
      
      val output = for (num <- re if (num % 2 == 0)) yield "Number " + num.toString
      
      val e = output.enumerate
      e.verify(List(
        _ == "Number 2",    
        _ == "Number 4"    
      ))
      
    }    
    
    "work with RefFutures successfully" in {
      
      val items = Enumerator(1, 2, 3, 4)
      val re = new RefEnumerator(items)
      
      def rf(num:Int) = {
        if (num % 2 == 0) {
          new RefFuture(scala.concurrent.future { "even: " + num })
        } else {
          new RefFutureOption(scala.concurrent.future { None })
        }
      }
      
      val output = for (num <- re; str <- rf(num)) yield "It was " + str
      
      val e = output.enumerate
      e.verify(List(
        _ == "It was even: 2",    
        _ == "It was even: 4"    
      ))
      
    }     
    
  }  
  
  
  "RefEnumIter" should {
    
    "enumerate each item" in {
      
      val items = Enumerator(List(1, 2, 3, 4).toIterator, List(11, 12, 13).toIterator)
      val re = new RefEnumIter(items)
      
      val output = for (num <- re) yield "Number " + num.toString
      
      val e = output.enumerate
      e.verify(List(
        _ == "Number 1",    
        _ == "Number 2",    
        _ == "Number 3",    
        _ == "Number 4",
        _ == "Number 11",
        _ == "Number 12",
        _ == "Number 13"
      ))
      
    }
    
  }
  
}
