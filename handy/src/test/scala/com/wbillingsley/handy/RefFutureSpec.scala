package com.wbillingsley.handy;

import org.specs2.mutable._
import Ref._
import org.specs2.concurrent.ExecutionEnv

import scala.concurrent.{ExecutionContext, Future, Promise}

class RefFutureSpec(implicit ee: ExecutionEnv) extends Specification {

  "RefFutures" should {

    "use an implicit execution context if one is in scope" in {

      val ec = new ExecutionContext {
        def execute(runnable:Runnable) = RefFuture.executionContext.execute(runnable)
        def reportFailure(t:Throwable) = RefFuture.executionContext.reportFailure(t)
      }

      ec.mustNotEqual(RefFuture.executionContext)

      val f = Future{ 4 }(ec)
      val rf = new RefFuture(f)(ec)

      rf.executionContext must be equalTo ec
    }

    "flatMap across single items" in {
      val futRef = Future { 3 }.toRef
      val after = futRef flatMap { i => (i + 1) itself }
      after.toFuture must be_==(4).await
    }

    "flatMap across plurals" in {

      val futRef = Future { 3 }.toRef

      val after = futRef flatMap { i =>
        List(1, 2, 3) toRefMany
      }
      after.collect.toFuture must be_==(Seq(1, 2, 3)).await
    }



  }



}
