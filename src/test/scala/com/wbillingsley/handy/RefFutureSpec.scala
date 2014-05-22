package com.wbillingsley.handy;

import org.specs2.mutable._
import Ref._
import scala.concurrent.{Future, Promise, ExecutionContext}

object RefFutureSpec extends Specification {

  "RefFutures" should {

    "use an implicit execution context if one is in scope" in {

      val ec = new ExecutionContext {
        def execute(runnable:Runnable) = RefFuture.executionContext.execute(runnable)
        def reportFailure(t:Throwable) = RefFuture.executionContext.reportFailure(t)
      }

      ec mustNotEqual(RefFuture.executionContext)

      val f = Future{ 4 }(ec)
      val rf = new RefFuture(f)(ec)

      rf.executionContext must be equalTo ec
    }

    "fetch simple futures to RefItself" in {
      import ExecutionContext.Implicits.global

      val futRef = Future { 3 }.toRef
      futRef.fetch must be_==(3.itself)
    }

    "flatMap across single items" in {
      import ExecutionContext.Implicits.global

      val futRef = Future { 3 }.toRef
      val after = futRef flatMap { i => (i + 1) itself }
      after.fetch must be_==(4.itself)
    }

    "flatMap across plurals" in {
      import ExecutionContext.Implicits.global

      val futRef = Future { 3 }.toRef

      val after = futRef flatMap { i =>
        List(1, 2, 3) toRefMany
      }
      after.fetch.toList must be_==(List(1, 2, 3))
    }

    "support onComplete" in {
      import ExecutionContext.Implicits.global

      val fut = Future { 5 }
      val futRef = fut.toRef
      var prom = Promise[Int]

      futRef.onComplete(
        onSuccess = { s =>
          prom.success(s)
        },
        onNone = { println("it was none") },
        onFail = { f => println("it failed") }
      )

      prom.future must be_==(5).await
    }



  }



}
