package com.wbillingsley.handy

import scala.concurrent.ExecutionContext

class RefIteratorSuite extends munit.FunSuite {

  case class Naturals(i:Int) extends RefIterator[Int] {
    def item = RefSome(i)
    def next = RefSome(Naturals(i + 1))
  }
  
  test("A consumer should be able to consume part of an infinite series") {
    val count = new RefConsumer[Int, String] {
      var traversed = 0

      override def process(b:String, ra:RefOpt[Int]) = {
        (for 
          a <- ra
        yield if a < 100000 then {
          traversed += 1
          ("ping", Some(this))
        } else (s"I traversed $traversed items", None)) orElse (b, None).itself
      }
    }
    
    given ec as ExecutionContext = ExecutionContext.global
    
    val done = RefIterator.process("")(Naturals(0), count)
    done.toFuture.map(x => assertEquals(x, "I traversed 100000 items"))
  }
  
  
}
