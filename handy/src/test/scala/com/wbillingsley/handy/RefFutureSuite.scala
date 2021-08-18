package com.wbillingsley.handy

import scala.concurrent.{ExecutionContext, Future}


class RefFutureSuite extends munit.FunSuite {

  test("RefFutureRefOpt recovers from failures in the outer Future") {

    val failed = RefFutureRefOpt(Future.failed(new IllegalStateException("test")))

    val f = for
      result <- failed recoverWith {
        case x: IllegalStateException => RefSome("yes")
        case x => RefOptFailed(x)
      }
    yield assertEquals(result, "yes")

    f.require.toFuture
  }


  test("RefFutureRefOpt recovers from failures in the inner RefOpt") {
    val failed = RefFutureRefOpt(Future.successful(RefOptFailed(new IllegalStateException("test"))))

    val f = for
      result <- failed recoverWith {
        case x: IllegalStateException => RefSome("yes")
        case x => RefOptFailed(x)
      }
    yield assertEquals(result, "yes")

    f.require.toFuture
  }

}
