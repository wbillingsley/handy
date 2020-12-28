package com.wbillingsley.handy

import scala.concurrent.{ExecutionContext, Future}


type Task[+T] = ExecutionContext => Ref[T]
type TaskOpt[+T] = ExecutionContext => RefOpt[T]
type TaskMany[+T] = ExecutionContext => RefMany[T]

implicit object Task {
  
  /** Used in order to launch tasks on an alternative execution context */
  private val done = Future.successful(())
  
  trait CFMT[-To[BB], +Result[BB]] {
    def flatMap[A, B](from:Task[A], to: A => To[B]):Result[B]
  }
  
  given FMTOne:CFMT[Task, Task] = new CFMT[Task, Task] {
    def flatMap[A, B](from:Task[A], to: A => Task[B]):Task[B] = from.flatMapOne(to)
  }

  given FMTOpt:CFMT[TaskOpt, TaskOpt] = new CFMT[TaskOpt, TaskOpt] {
    def flatMap[A, B](from:Task[A], to: A => TaskOpt[B]):TaskOpt[B] = from.flatMapOpt(to)
  }

  given FMTMany:CFMT[TaskMany, TaskMany] = new CFMT[TaskMany, TaskMany] {
    def flatMap[A, B](from:Task[A], to: A => TaskMany[B]):TaskMany[B] = from.flatMapMany(to)
  }


  extension[T] (task: Task[T]) {
    def runAsync(using ec:ExecutionContext):Ref[T] = task(ec)
  }

  extension[T, B] (task: Task[T]) {
    def map(f: T => B): Task[B] = { (ec: ExecutionContext) =>
      task(ec).map(f)
    }

    def flatMapOne(second: T => Task[B]): Task[B] = { (ec: ExecutionContext) =>
      task(ec).flatMapOne((res) => second(res)(ec))
    }

    def flatMapOpt(second: T => TaskOpt[B]): TaskOpt[B] = { (ec: ExecutionContext) =>
      task(ec).flatMapOpt((res) => second(res)(ec))
    }

    def flatMapMany(second: T => TaskMany[B]): TaskMany[B] = { (ec: ExecutionContext) =>
      task(ec).flatMapMany((res) => second(res)(ec))
    }
  }
  
  extension[T, B, R[_], Result[_]] (task:Task[T]) {
    def flatMap(func: T => R[B])(implicit imp: CFMT[R, Result]):Result[B] = {
      imp.flatMap(task, func)
    }
  }
  
  /** Creates a Task[T] that will run on the execution context it is passed */
  def prepare[T](f: => Ref[T]):Task[T] = (ec) => (new RefFuture(done)(ec)).flatMapOne(_ => f)

  /** Creates a Task[T] that will run on the execution context it is passed */
  def prepareOpt[T](f: => RefOpt[T]):TaskOpt[T] = (ec) => (new RefFuture(done)(ec)).flatMapOpt(_ => f)

  /** Creates a Task[T] that will run on the execution context it is passed */
  def prepareMany[T](f: => RefMany[T]):TaskMany[T] = (ec) => (new RefFuture(done)(ec)).flatMapMany(_ => f)
  
}

implicit object TaskOpt {

  trait CFMT[-To[BB], +Result[BB]] {
    def flatMap[A, B](from:TaskOpt[A], to: A => To[B]):Result[B]
  }

  given FMTOne:CFMT[Task, TaskOpt] = new CFMT[Task, TaskOpt] {
    def flatMap[A, B](from:TaskOpt[A], to: A => Task[B]):TaskOpt[B] = from.flatMapOne(to)
  }

  given FMTOpt:CFMT[TaskOpt, TaskOpt] = new CFMT[TaskOpt, TaskOpt] {
    def flatMap[A, B](from:TaskOpt[A], to: A => TaskOpt[B]):TaskOpt[B] = from.flatMapOpt(to)
  }

  extension[T] (task: TaskOpt[T]) {
    def runAsync(using ec:ExecutionContext):RefOpt[T] = task(ec)
  }
  
  extension[T, B] (task: TaskOpt[T]) {
    def map(f: T => B): TaskOpt[B] = { (ec: ExecutionContext) =>
      task(ec).map(f)
    }

    def flatMapOne(second: T => Task[B]): TaskOpt[B] = { (ec: ExecutionContext) =>
      task(ec).flatMapOne((res) => second(res)(ec))
    }

    def flatMapOpt(second: T => TaskOpt[B]): TaskOpt[B] = { (ec: ExecutionContext) =>
      task(ec).flatMapOpt((res) => second(res)(ec))
    }

    def flatMapMany(second: T => TaskMany[B]): TaskMany[B] = { (ec: ExecutionContext) =>
      task(ec).flatMapMany((res) => second(res)(ec))
    }
  }

  extension[T, B, R[_], Result[_]] (task:TaskOpt[T]) {
    def flatMap(func: T => R[B])(implicit imp: CFMT[R, Result]):Result[B] = {
      imp.flatMap(task, func)
    }
  }
}

implicit object TaskMany {

  trait CFMT[-To[BB], +Result[BB]] {
    def flatMap[A, B](from:TaskMany[A], to: A => To[B]):Result[B]
  }

  given FMTOne:CFMT[Task, TaskMany]  = new CFMT[Task, TaskMany] {
    def flatMap[A, B](from:TaskMany[A], to: A => Task[B]):TaskMany[B] = from.flatMapOne(to)
  }

  given FMTOpt:CFMT[TaskOpt, TaskMany] = new CFMT[TaskOpt, TaskMany] {
    def flatMap[A, B](from:TaskMany[A], to: A => TaskOpt[B]):TaskMany[B] = from.flatMapOpt(to)
  }

  given FMTMany:CFMT[TaskMany, TaskMany] = new CFMT[TaskMany, TaskMany] {
    def flatMap[A, B](from:TaskMany[A], to: A => TaskMany[B]):TaskMany[B] = from.flatMapMany(to)
  }

  extension[T] (task: TaskMany[T]) {
    def runAsync(using ec:ExecutionContext):RefMany[T] = task(ec)
  }

  extension[T, B] (task: TaskMany[T]) {
    def map(f: T => B): TaskMany[B] = { (ec: ExecutionContext) =>
      task(ec).map(f)
    }

    def flatMapOne(second: T => Task[B]): TaskMany[B] = { (ec: ExecutionContext) =>
      task(ec).flatMapOne((res) => second(res)(ec))
    }

    def flatMapOpt(second: T => TaskOpt[B]): TaskMany[B] = { (ec: ExecutionContext) =>
      task(ec).flatMapOpt((res) => second(res)(ec))
    }

    def flatMapMany(second: T => TaskMany[B]): TaskMany[B] = { (ec: ExecutionContext) =>
      task(ec).flatMapMany((res) => second(res)(ec))
    }
  }

  extension[T, B, R[_], Result[_]] (task:TaskMany[T]) {
    def flatMap(func: T => R[B])(implicit imp: CFMT[R, Result]):Result[B] = {
      imp.flatMap(task, func)
    }
  }
}




