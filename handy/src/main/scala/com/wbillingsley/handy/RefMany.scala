package com.wbillingsley.handy

import scala.collection.mutable
import scala.language.higherKinds

/**
 * A reference that is expected to contain several items
 */
trait RefMany[+T] extends RSuper[T] {

  def fetch:ResolvedRefMany[T]

  def map[B](func: T => B):RefMany[B]

  def foreach[U](func: T => U):Unit

  def flatMap[B, R[B], Result[B]](func: T => R[B])(implicit imp: RMCFMT[RefMany, R, Result]):Result[B] = imp.flatMap(this, func)

  def flatMapOne[B](func: T => Ref[B]):RefMany[B]

  def flatMapMany[B](func: T => RefMany[B]):RefMany[B]

  def withFilter(func: T => Boolean):RefMany[T]

  def first:Ref[T]

  /**
   * A fold across this (possibly asynchronous) collection
   * initial will only be evaluated in the success case.
   */
  def fold[B](initial: =>B)(each:(B, T) => B):Ref[B]

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
  def toRefOne:Ref[TraversableOnce[T]] = collect

  def collect:Ref[Seq[T]] = {
    fold(mutable.Buffer.empty[T]){(buf, item) => buf.append(item); buf}
  }

}

object RefMany {

  /**
   * FlatMap from many to many returns a RefMany.
   */
  implicit object ManyToMany extends RMCFMT[RefMany, RefMany, RefMany] {
    def flatMap[A, B](from: RefMany[A], f: A => RefMany[B]) = {
      from.flatMapMany(f)
    }
  }


  /**
   * FlatMap from many to one returns a RefMany.
   */
  implicit object ManyToOne extends RMCFMT[RefMany, Ref, RefMany] {
    def flatMap[A, B](from: RefMany[A], f: A => Ref[B]) = {
      from.flatMapOne(f)
    }
  }
}
