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
    val after = futRef flatMap { i => List(1, 2, 3) toRef }
    assertEquals(List(1, 2, 3), after.fetch.toList)        
  }
}