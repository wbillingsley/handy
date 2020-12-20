package com.wbillingsley.handy

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

/**
  * Composable equivalent to `Future[Option[T]]`
  * @tparam T the contained item type
  */
trait RefOpt[+T] extends RSuper[T] {

  def require:Ref[T]

  /** map, as in functors */
  def map[B](func: T => B):RefOpt[B]

  def foreach(func: T => Unit):Unit = {
    this.map(func)
  }

  /** Bind, as in monads */
  def bind[B](func: T => RefOpt[B]):RefOpt[B] = flatMapOpt(func)

  /**
    * Allows use in for comprehensions.
    * Automatically routes to flatMapOne, flatMapOpt (bind), or flatMapMany
    */
  def flatMap[B, R[_], Result[_]](func: T => R[B])(implicit imp: RCFMT[RefOpt, R, Result]):Result[B] = {
    imp.flatMap(this, func)
  }

  /** Allows for comprehensions involving Ref[T] */
  def flatMapOne[B](func: T => Ref[B]):RefOpt[B]

  def flatMap[B](func: T => Ref[B])(implicit ev: RefOpt.FMOne.type):RefOpt[B] = flatMapOne(func)

  /** Also known as bind */
  def flatMapOpt[B](func: T => RefOpt[B]):RefOpt[B]

  def flatMap[B](func: T => RefOpt[B])(implicit ev: RefOpt.FMOpt.type):RefOpt[B] = flatMapOpt(func)

  /** Allows for comprehensions involving RefMany[T] */
  def flatMapMany[B](func: T => RefMany[B]):RefMany[B]

  def flatMap[B](func: T => RefMany[B])(implicit ev: RefOpt.FMMany.type):RefMany[B] = flatMapMany(func)

  /** Convert to Scala library types */
  def toFutureOpt:Future[Option[T]]

  /** If the RefOpt evaluates to not containing a value, f will be produced instead */
  def orElse[B >: T](f: => RefOpt[B]):RefOpt[B]

  def orElse[B >: T](f: => Ref[B]):Ref[B] = orElse(f.toRefOpt).require

  def orFail(f: => Throwable) = orElse(RefFailed(f))

  def option:Ref[Option[T]]

  /** For recovery from failures */
  def recoverWith[B >: T](pf: PartialFunction[Throwable, RefOpt[B]]):RefOpt[B]

  def withFilter(pred: T => Boolean):RefOpt[T]

}

object RefOpt {
  import scala.concurrent.{ExecutionContext, Future}

  def apply[T](opt:Option[T]):RefOptSync[T] = opt match {
    case Some(v) => RefSome(v)
    case None => RefNone
  }

  def fromFutureOpt[T](fo:Future[Option[T]])(implicit ec:ExecutionContext):RefOpt[T] = {
    new RefFuture(fo)(ec).flatMap((x) => RefOpt.apply(x))
  }

  def none:RefOpt[Nothing] = RefNone

  def some[T](v:T):RefSome[T] = RefSome(v)

  def pure[T](v:T):RefOpt[T] = some(v)

  /**
    * FlatMap from one to one returns a singular Ref.
    */
  implicit object OptToOne extends RCFMT[RefOpt, Ref, RefOpt] {
    def flatMap[A, B](from: RefOpt[A], f: A => Ref[B]):RefOpt[B] = {
      from.flatMapOne(f)
    }
  }

  /**
    * FlatMap from one to option returns a singular Ref.
    */
  implicit object OptToOpt extends RCFMT[RefOpt, RefOpt, RefOpt] {
    def flatMap[A, B](from: RefOpt[A], f: A => RefOpt[B]):RefOpt[B] = {
      from.flatMapOpt(f)
    }
  }

  /**
    * FlatMap from one to many returns a RefMany.
    */
  implicit object OptToMany extends RCFMT[RefOpt, RefMany, RefMany] {
    def flatMap[A, B](from: RefOpt[A], f: A => RefMany[B]):RefMany[B] = {
      from.flatMapMany(f)
    }
  }

  implicit object FMOne
  implicit object FMOpt
  implicit object FMMany

}

/**
  * A RefOpt that is synchronously available
  * @tparam T the contained item type
  */
trait RefOptSync[+T] extends RefOpt[T] {

  def toEither:Either[Throwable, Option[T]]

  def toTry:Try[Option[T]]

}

/**
  * The empty RefOpt
  */
case object RefNone extends RefOptSync[Nothing] {

  override def require = RefFailed(new NoSuchElementException("require was called on an empty RefOpt"))

  override def toEither = Right(None)

  override def toTry = Success(None)

  override def toFutureOpt:Future[Option[Nothing]] = Future.successful(None)

  override def orElse[B >: Nothing](f: => RefOpt[B]):RefOpt[B] = f

  override def flatMapOne[B](func: Nothing => Ref[B]):RefOpt[B] = this

  override def flatMapOpt[B](func: Nothing => RefOpt[B]):RefOpt[B] = this

  override def flatMapMany[B](func: Nothing => RefMany[B]):RefMany[B] = RefEmpty

  override def map[B](func: Nothing => B): RefOpt[B] = this

  override def recoverWith[B >: Nothing](pf: PartialFunction[Throwable, RefOpt[B]]): RefOpt[Nothing] = this

  override def withFilter(pred: Nothing => Boolean):RefOpt[Nothing] = this

  override def option: Ref[Option[Nothing]] = RefItself(None)
}

/**
  * A RefOpt representing Some(v)
  * @param v the value
  * @tparam T contained item type
  */
case class RefSome[+T](v:T) extends RefOptSync[T] {

  override def require:Ref[T] = RefItself(v)

  override def toEither: Either[Throwable, Option[T]] = Right(Some(v))

  override def toTry: Try[Option[T]] = Success(Some(v))

  override def map[B](func: T => B): RefOpt[B] = RefSome(func(v))

  override def flatMapOne[B](func: T => Ref[B]): RefOpt[B] = func(v).flatMapOpt(RefSome.apply)

  override def flatMapOpt[B](func: T => RefOpt[B]): RefOpt[B] = func(v)

  override def flatMapMany[B](func: T => RefMany[B]): RefMany[B] = func(v)

  override def toFutureOpt: Future[Option[T]] = Future.successful(Some(v))

  override def orElse[B >: T](f: => RefOpt[B]): RefOpt[B] = this

  override def recoverWith[B >: T](pf: PartialFunction[Throwable, RefOpt[B]]): RefOpt[T] = this

  override def withFilter(pred: T => Boolean):RefOpt[T] = if (pred(v)) this else RefNone

  override def option: Ref[Option[T]] = RefItself(Some(v))
}

/**
  * A RefOpt representing a failure
  * @param t the failure
  */
case class RefOptFailed(t:Throwable) extends RefOpt[Nothing] with RefOptSync[Nothing] {

  override def require:Ref[Nothing] = RefFailed(t)

  override def map[B](func: Nothing => B): RefOpt[B] = this

  override def flatMapOne[B](func: Nothing => Ref[B]): RefOpt[B] = this

  override def flatMapOpt[B](func: Nothing => RefOpt[B]): RefOpt[B] = this

  override def flatMapMany[B](func: Nothing => RefMany[B]): RefMany[B] = RefManyFailed(t)

  override def toFutureOpt: Future[Option[Nothing]] = Future.failed(t)

  override def orElse[B >: Nothing](f: => RefOpt[B]):RefOpt[B] = this

  override def recoverWith[B >: Nothing](pf: PartialFunction[Throwable, RefOpt[B]]):RefOpt[B] = {
    pf.applyOrElse(t, { (_:Throwable) => this })
  }

  override def toEither: Either[Throwable, Option[Nothing]] = Left(t)

  override def toTry: Try[Option[Nothing]] = Failure(t)

  override def withFilter(pred: Nothing => Boolean):RefOpt[Nothing] = this

  override def option: Ref[Option[Nothing]] = RefFailed(t)
}
