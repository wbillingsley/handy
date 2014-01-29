package com.wbillingsley.handy;

import org.specs2.mutable._
import Ref._

object RefFutureSpec extends Specification {

  "RefFutures" should {

    "use an implicit execution context if one is in scope" in {
      import scala.concurrent._

      implicit val ec = new ExecutionContext {
        def execute(runnable:Runnable) = RefFuture.executionContext.execute(runnable)
        def reportFailure(t:Throwable) = RefFuture.executionContext.reportFailure(t)
      }

      ec mustNotEqual(RefFuture.executionContext)

      val f = future { 4 }
      val rf = new RefFuture(f)

      rf.executionContext must be equalTo ec
    }


  }



}
