/**
 *
 */
package com.wbillingsley.handy

import org.junit.{BeforeClass, Test}
import org.junit.Assert._

import scala.collection.mutable
import scala.language.implicitConversions
import Ref._
import RefMany._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable._



class RefIterableOnceSpec(implicit ee: ExecutionEnv) extends Specification {
  
  "RefTraversableOnce" should {

    "support simple cross-products" in {
      val rnum = 3.itself

      val rlistNum = List(1, 2, 3).toRefMany

      val rlistStr = List("one", "two", "three").toRefMany

      val cross = rlistNum flatMap { _ => rlistStr }

      cross.collect.toFuture must be_==(List("one", "two", "three", "one", "two", "three", "one", "two", "three")).await
    }

    "support flatMapOne" in {
      val rnum = 3.itself

      val rlistNum = List(1, 2, 3).toRefMany

      val cross = rlistNum flatMap {i => rnum}

      cross.collect.toFuture must be_==(List(3, 3, 3)).await

    }

    "flatMap with RefNone" in {
      val rlistNum:RefMany[Int] = List(1, 2, 3).toRefMany

      val cross = rlistNum flatMap {i => RefNone}
      val count = cross.foldLeft(0) { (tot, el) => tot + 1}

      count.toFuture must be_==(0).await
    }

  }

}