package com.wbillingsley.handy

import com.wbillingsley.handy.reactivestreams.TakeWhileR
import org.reactivestreams.Publisher

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

/**
 * A reference that is expected to contain several items
 */
trait RefMany[+T] extends RSuper[T] {

  /** map, as in functors */
  def map[B](func: T => B):RefMany[B]

  /** bind, as in monads */
  def bind[B](f: T => RefMany[B]):RefMany[B] = flatMapMany(f)

  def foreach[U](func: T => U):Unit

  def flatMap[B, R[_], Result[_]](func: T => R[B])(implicit imp: RCFMT[RefMany, R, Result]):Result[B] = imp.flatMap(this, func)

  def flatMapOne[B](func: T => Ref[B]):RefMany[B]

  def flatMapOpt[B](func: T => RefOpt[B]):RefMany[B]

  def flatMapMany[B](func: T => RefMany[B]):RefMany[B]

  def withFilter(func: T => Boolean):RefMany[T]

  def first:RefOpt[T]

  /**
   * A fold across this (possibly asynchronous) collection
   * initial will only be evaluated in the success case.
   */
  def foldLeft[B](initial: =>B)(each:(B, T) => B):Ref[B]

  /**
   * Recovers from failures producing the list -- for instance if this is a RefFailed, or a RefFutureRefMany that fails.
   * Note that it does not recover from individual elements within the list failing.
   */
  def recoverManyWith[B >: T](pf: PartialFunction[Throwable, RefMany[B]]):RefMany[B]

  /**
   * Called when the RefMany is "ready". This is equivalent to
   * fold(initial){ (_,_) => initial } but without calling the empty folder for each value
   */
  def whenReady[B](f: RefMany[T] => B):Ref[B]

  /**
   * Converts this to a reference to a collection
   */
  def collect:Ref[Seq[T]] = {
    foldLeft(mutable.Buffer.empty[T]) {
      (buf, item) => buf.append(item)
      buf
    }.map(_.toSeq)
  }

}

trait RefManySync[+T] extends RefMany[T] {

  def toSeq:Ref[Seq[T]]

}

object RefMany {

  implicit object FMOne
  implicit object FMOpt
  implicit object FMMany

  /**
   * FlatMap from many to many returns a RefMany.
   */
  implicit object ManyToMany extends RCFMT[RefMany, RefMany, RefMany] {
    def flatMap[A, B](from: RefMany[A], f: A => RefMany[B]):RefMany[B] = {
      from.flatMapMany(f)
    }
  }

  /**
    * FlatMap from many to many returns a RefMany.
    */
  implicit object ManyToOpt extends RCFMT[RefMany, RefOpt, RefMany] {
    def flatMap[A, B](from: RefMany[A], f: A => RefOpt[B]):RefMany[B] = {
      from.flatMapOpt(f)
    }
  }

  /**
   * FlatMap from many to one returns a RefMany.
   */
  implicit object ManyToOne extends RCFMT[RefMany, Ref, RefMany] {
    def flatMap[A, B](from: RefMany[A], f: A => Ref[B]):RefMany[B] = {
      from.flatMapOne(f)
    }
  }


  implicit class PubOps[T](val rm:Publisher[T]) extends AnyVal {
    def toRefMany(implicit ec:ExecutionContext):RefMany[T] = new RefPublisher(rm)
  }

  implicit class RMStreamOps[T](val rm:RefMany[T]) extends AnyVal {

    def takeWhile(func: (T) => Boolean)(implicit ec:ExecutionContext): RefMany[T] = {
      val pub = new RMPublisher[T](rm)
      val tw  = new TakeWhileR[T](pub)(func.andThen(RefItself.apply))
      new RefPublisher[T](tw)
    }

  }


}

/**
  * A ref which has failed before it has even begun to stream
  * @param throwable the failure
  */
case class RefManyFailed(throwable: Throwable) extends RefMany[Nothing] with RefManySync[Nothing] {

  override def map[B](func: Nothing => B): RefMany[B] = this

  override def foreach[U](func: Nothing => U): Unit = {}

  override def flatMapOne[B](func: Nothing => Ref[B]): RefMany[B] = this

  override def flatMapOpt[B](func: Nothing => RefOpt[B]): RefMany[B] = this

  override def flatMapMany[B](func: Nothing => RefMany[B]): RefMany[B] = this

  override def withFilter(func: Nothing => Boolean): RefMany[Nothing] = this

  override def first: RefOpt[Nothing] = RefOptFailed(throwable)

  /**
    * A fold across this (possibly asynchronous) collection
    * initial will only be evaluated in the success case.
    */
  override def foldLeft[B](initial: => B)(each: (B, Nothing) => B): Ref[B] = RefFailed(throwable)

  /**
    * Recovers from failures producing the list -- for instance if this is a RefFailed, or a RefFutureRefMany that fails.
    * Note that it does not recover from individual elements within the list failing.
    */
  override def recoverManyWith[B >: Nothing](pf: PartialFunction[Throwable, RefMany[B]]): RefMany[B] = {
    pf.applyOrElse(throwable, { _:Throwable => this })
  }

  /**
    * Called when the RefMany is "ready". This is equivalent to
    * fold(initial){ (_,_) => initial } but without calling the empty folder for each value
    */
  override def whenReady[B](f: RefMany[Nothing] => B): Ref[B] = RefFailed(throwable)

  override def toSeq: Ref[Seq[Nothing]] = RefFailed(throwable)

}

object RefEmpty extends RefManySync[Nothing] {

  override def toSeq: Ref[Seq[Nothing]] = RefItself(Seq.empty)

  override def map[B](func: Nothing => B): RefMany[B] = this

  override def foreach[U](func: Nothing => U): Unit = {}

  override def flatMapOne[B](func: Nothing => Ref[B]): RefMany[B] = this

  override def flatMapOpt[B](func: Nothing => RefOpt[B]): RefMany[B] = this

  override def flatMapMany[B](func: Nothing => RefMany[B]): RefMany[B] = this

  override def withFilter(func: Nothing => Boolean): RefMany[Nothing] = this

  override def first: RefOpt[Nothing] = RefNone

  /**
    * A fold across this (possibly asynchronous) collection
    * initial will only be evaluated in the success case.
    */
  override def foldLeft[B](initial: => B)(each: (B, Nothing) => B): Ref[B] = RefItself(initial)

  /**
    * Recovers from failures producing the list -- for instance if this is a RefFailed, or a RefFutureRefMany that fails.
    * Note that it does not recover from individual elements within the list failing.
    */
  override def recoverManyWith[B >: Nothing](pf: PartialFunction[Throwable, RefMany[B]]): RefMany[B] = this

  /**
    * Called when the RefMany is "ready". This is equivalent to
    * fold(initial){ (_,_) => initial } but without calling the empty folder for each value
    */
  override def whenReady[B](f: RefMany[Nothing] => B): Ref[B] = RefItself(f(this))
}
