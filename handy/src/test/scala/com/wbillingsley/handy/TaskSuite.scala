package com.wbillingsley.handy

import scala.concurrent.{ExecutionContext, Future}

class TaskSuite extends munit.FunSuite {
  
  /** A completed future, so that we can schedule a future to take place on another thread */
  private val done = Future.successful(())
  
  /** Produces a Ref[T] from a computation that will occur on the given execution context */
  def schedule[T](f: => T):ExecutionContext => RefFuture[T] = { (ec) =>
    RefFuture(done.map(_ => f)(using ec))(ec)
  }

  /** Produces a RefOpt[T] from a computation that will occur on the given execution context */
  def scheduleOpt[T](f: => Option[T]):ExecutionContext => RefOpt[T] = { (ec) =>
    RefFuture(done)(using ec).flatMapOpt(_ => RefOpt(f))
  }

  /** Produces a RefMany[T] from a computation that will occur on the given execution context */
  def scheduleMany[T](f: => Seq[T]):ExecutionContext => RefMany[T] = { (ec) =>
    RefFuture(done)(using ec).flatMapMany(_ => RefIterableOnce(f))
  }

  test("Task flatMapOne Task produces Task") {
    val t = schedule({ 1 }).flatMapOne({ (x) => schedule { x + 1 }})
    
    for result <- t.runAsync(using ExecutionContext.global) yield
      assertEquals(result, 2)
  }

  test("Task for notation with Task produces Task") {
    val task = for {
      a <- schedule { 1 }
      b <- schedule { a + 1 }
    } yield b
    
    for result <- task.runAsync(using ExecutionContext.global) do {
      assertEquals(result, 2)
    }     
  }

  test("Task flatMapOpt TaskOpt produces TaskOpt") {
    val t:TaskOpt[Int] = schedule({ 1 }).flatMapOpt({ (x) => scheduleOpt { Some(x + 1) }})

    for result <- t.runAsync(using ExecutionContext.global).option yield
      assertEquals(result, Some(2))
  }

  test("Task for notation with TaskOpt produces TaskOpt") {
    val task = for {
      a <- schedule { 1 }
      b <- scheduleOpt { Some(a + 1) }
    } yield b

    for result <- task.runAsync(using ExecutionContext.global).option yield
      assertEquals(result, Some(2))
  }

  test("Task flatMapMany TaskMany produces TaskMany") {
    val t:TaskMany[Int] = schedule({ 1 }).flatMapMany({ (x) => scheduleMany { Seq(x, x) }})

    for result <- t.runAsync(using ExecutionContext.global).collect yield
      assertEquals(result, Seq(1, 1))
  }

  test("Task for notation with TaskMany produces TaskMany") {
    val task = for {
      a <- schedule { 1 }
      b <- scheduleMany { Seq(a, a) }
    } yield b

    for result <- task.runAsync(using ExecutionContext.global).collect yield
      assertEquals(result, Seq(1, 1))
  }
  
}
