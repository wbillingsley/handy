package com.wbillingsley.handy

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Failure, Success}

/**
  * This test suite mostly tests the extension methods compile.
  */
class RefOpsSuite extends munit.FunSuite {
  
  test("Item.itself is a Ref[Item]") {
    assertEquals(1.itself, RefItself(1))
  }
  
  test("Some(t).toRefOpt is RefSome(t)") {
    assertEquals(Some(1).toRefOpt, RefSome(1))
  }

  test("Success(t).toRef is RefItself(t)") {
    assertEquals(Success(1).toRef, RefItself(1))
  }

  test("Failure(exc).toRef is RefFailed(exc)") {
    assertEquals(Failure(Refused("No")).toRef, RefFailed(Refused("No")))
  }

  test("throwable.toRef is RefFailed(exc)") {
    assertEquals(Refused("No").toRef, RefFailed(Refused("No")))
  }

  test("throwable.toRefOpt is RefOptFailed(exc)") {
    assertEquals(Refused("No").toRefOpt, RefOptFailed(Refused("No")))
  }

  test("Future[T].toRef compiles") {
    given ec: ExecutionContext = ExecutionContext.global

    (for v <- Future.apply(1).toRef yield assertEquals(v, 1)).toFuture
  }

  test("Future[Option[T]].toRefOpt compiles") {
    given ec: ExecutionContext = ExecutionContext.global

    (for v <- Future.apply(Some(1)).toRefOpt yield assertEquals(v, 1)).require.toFuture
  }
  
}
