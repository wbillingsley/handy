package com.wbillingsley.handy

import org.junit.{BeforeClass, Test}
import org.junit.Assert._
import scala.collection.mutable
import Ref._

object TestApproval {
  case class User(name:String)	
}

import TestApproval._


case class CanDoOdd(i: Int) extends Perm[User] {
  
  def resolve(prior: Approval[User]) = {
    if (i % 2 ==1) {
      Approved("Yes, it was odd")
    } else {
      Refused("No, it was even")
    }
  }
  
}

class TestApproval {

  val fred = User("fred")
  
  @Test
  def basic {
    val a = Approval(fred.itself)    
    assertEquals(RefItself(Approved("Yes, it was odd")), a ask CanDoOdd(1))    
  }
  
}