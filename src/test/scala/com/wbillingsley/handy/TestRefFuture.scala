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

class TestRefFuture {

  @Test
  def simpleFuture {    
    val futRef = future { 3 }.toRef    
    assertEquals(RefItself(3), futRef.fetch)    
  }
  
  @Test
  def flatMap {
    val futRef = future { 3 }.toRef
    val after = futRef flatMap { i => (i + 1) itself }
    assertEquals(RefItself(4), after.fetch)    
  }
  
  @Test
  def flatMapTrav {
    val futRef = future { 3 }.toRef
    
    val after = futRef flatMap { i =>       
      List(1, 2, 3) toRefMany
    }
    assertEquals(List(1, 2, 3), after.fetch.toList)        
  }
  
  @Test
  def complete {
    val fut = future { 5 } 
    val futRef = fut.toRef
    var a = 0
    
    futRef.onComplete(
        onSuccess = { s =>
          println("Setting it to " + s)
          a = s
          println("done it")
        },
        onNone = { println("it was none") },
        onFail = { f => println("it failed") }
    )
    
    /*
     * This might be a shaky test -- 
     * it relies on the futRef.onComplete having happened first
     * (essentially relying on us having created a future that is
     *  already completed) 
     */
    fut.onComplete(v => assertEquals(5, a))
    
  }
}