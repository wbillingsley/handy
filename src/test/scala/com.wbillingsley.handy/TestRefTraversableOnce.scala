/**
 *
 */
package com.wbillingsley.handy

import org.junit.{BeforeClass, Test}
import org.junit.Assert._
import scala.collection.mutable

import scala.language.implicitConversions
import Ref._



class TestRefTraversableOnce {
  
  @Test
  def simpleCross {
	  val rnum = 3 itself
	  
	  val rlistNum = List(1, 2, 3) toRef
	  
	  val rlistStr = List("one", "two", "three") toRef
	  
	  val cross = (rlistNum flatMap {i => rlistStr})
	  val list = cross.fetch.toList
	  
	  assertEquals(List("one", "two", "three", "one", "two", "three", "one", "two", "three"), list)        
  }
  
  @Test
  def crossResolved {
	  val rnum = 3 itself
	  
	  val rlistNum = List(1, 2, 3) toRef
    
	  val cross = (rlistNum flatMap {i => rnum})
	  val list = cross.fetch.toList

	  assertEquals(List(3, 3, 3), list)        
  }
  
  @Test
  def crossNoneIsEmpty {	  
	  val rlistNum = List(1, 2, 3) toRef
    
	  val cross = (rlistNum flatMap {i => RefNone})

	  assertEquals(true, cross.isEmpty)        
  }
  
  @Test
  def fetchedNoneFilters {	  
	  val rlistNum = List(1, 2, 3) toRef
    
	  val cross = (rlistNum flatMap {i => if (i % 2 == 1) i itself else RefNone})

	  assertEquals(List(1, 3), cross.fetch.toList)        
  }
}