package com.wbillingsley.handy

import org.junit.{BeforeClass, Test}
import org.junit.Assert._
import scala.collection.mutable

object TestPerm {

}

class TestPerm {

  @Test
  def performResolvedRef() = {
    val r = RefItself("hello")
    val fred = "Fred"
    val pt = new PermissionToken(fred)

    val result = pt.perform(r.map(_ + "there"))
    assertTrue("Wrong type", result.isInstanceOf[ResolvedRef[String]])
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