package com.wbillingsley.handy

import org.junit.{BeforeClass, Test}
import org.junit.Assert._
import scala.collection.mutable


case class User(name:String)

object TestApproval {
  
  
  
}

class TestApproval {

  val fred = User("fred")
  
  @Test
  def default = {
    
    val a:Approval[Ref[User]] = Approval(RefItself(fred))
    
    val ra:Ref[Approval[Ref[User]]] = a
    
    
    
  }

  @Test
  def performUnresolvedRef() = {
    val r = RefById(classOf[String], "id")
    val fred = "Fred"
    val pt = new PermissionToken(fred)

    val result = pt.perform({ r })
    assertTrue("Wrong type", result.isInstanceOf[Ref[String]])
    assertTrue("Wrong type", !result.isInstanceOf[ResolvedRef[String]])
  }
  
  
}