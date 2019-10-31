package com.wbillingsley.handy

import com.wbillingsley.handy.reactivestreams.TakeWhileR

import scala.annotation.tailrec
import scala.language.{higherKinds, postfixOps}

/**
 * A reference to a collection of items.
 */
case class RefIterableOnce[T, C[TT] <: IterableOnce[TT]](items: C[T]) extends RefManySync[T] {

  override def first:RefOpt[T] = RefOpt(headOption)

  def headOption:Option[T] = items.iterator.to(LazyList).headOption

  override def map[B](f: T => B):RefManySync[B] = {
    val result = for (item <- items.iterator) yield f(item)
    RefIterableOnce(result)
  }

  override def flatMapMany[B](f: T => RefMany[B]) = RefIterableRefMany(items.iterator map f)

  override def flatMapOpt[B](f: T => RefOpt[B]):RefMany[B] = RefIterableRefOpt(items.iterator map f)

  override def flatMapOne[B](f: T => Ref[B]) = RefIterableRef(items.iterator map f)

  override def foreach[U](f: T => U):Unit = { items.iterator.foreach(f) }

  override def recoverManyWith[B >: T](pf: PartialFunction[Throwable, RefMany[B]]):RefMany[B] = this

  override def withFilter(p: T => Boolean) = RefIterableOnce(items.iterator withFilter p)

  override def foldLeft[B](initial: =>B)(each:(B, T) => B) = RefItself(items.iterator.foldLeft(initial)(each))

  override def whenReady[B](block: RefMany[T] => B):Ref[B] = RefItself(block(this))

  override def collect = RefItself(items.iterator.toSeq)

  override def toSeq: Ref[Seq[T]] = collect
}

case class RefIterableRef[T, C[TT] <: IterableOnce[TT]](refs: IterableOnce[Ref[T]]) extends RefMany[T] {

  override def first:RefOpt[T] = RefOpt(refs.iterator.to(Iterable).headOption).flatMapOne(identity)

  override def map[B](f: T => B):RefMany[B] = {
    val result = refs.iterator.map(_.map(f))
    RefIterableRef(result)
  }

  override def flatMapOne[B](f: T => Ref[B]):RefMany[B] = {
    val result = refs.iterator.map(_.flatMap(f))
    RefIterableRef(result)
  }

  override def flatMapOpt[B](func: T => RefOpt[B]): RefMany[B] = {
    val result = refs.iterator.map(_.flatMap(func))
    RefIterableRefOpt(result)
  }

  override def flatMapMany[B](f: T => RefMany[B]):RefMany[B] = {
    val result = refs.iterator.map(_.flatMap(f))
    RefIterableRefMany(result)
  }

  override def foreach[U](f: T => U):Unit = { refs.iterator.foreach(_.foreach(f)) }

  override def withFilter(p: T => Boolean):RefMany[T] = {
    flatMapOpt(x => if (p(x)) RefSome(x) else RefNone)
  }

  override def foldLeft[B](initial: =>B)(each:(B, T) => B):Ref[B] = {

    refs.iterator.foldLeft[Ref[B]](RefItself(initial)){(ar, br) =>
      for (a <- ar; ob <- br) yield each(a, ob)
    }
  }

  override def whenReady[B](block: RefMany[T] => B):Ref[B] = RefItself(block(this))

  override def recoverManyWith[B >: T](pf: PartialFunction[Throwable, RefMany[B]]):RefMany[T] = this

}

case class RefIterableRefOpt[+T](refs: IterableOnce[RefOpt[T]]) extends RefMany[T] {

  /** map, as in functors */
  override def map[B](func: T => B): RefMany[B] = {
    RefIterableRefOpt(refs.iterator.map(_.map(func)))
  }

  override def foreach[U](func: T => U): Unit = map(func)

  override def flatMapOne[B](func: T => Ref[B]): RefMany[B] = {
    RefIterableRefOpt(refs.iterator.map(_.flatMapOne(func)))
  }

  override def flatMapOpt[B](func: T => RefOpt[B]): RefMany[B] = {
    RefIterableRefOpt(refs.iterator.map(_.flatMapOpt(func)))
  }

  override def flatMapMany[B](func: T => RefMany[B]): RefMany[B] = {
    RefIterableRefMany(refs.iterator.map(_.flatMapMany(func)))
  }

  override def withFilter(func: T => Boolean): RefMany[T] = {
    RefIterableRefOpt(refs.iterator.map(_.withFilter(func)))
  }

  /** TODO: this consumes the whole stream */
  override def first: RefOpt[T] = refs.iterator.foldLeft[RefOpt[T]](RefNone) { (opt, ro) => opt orElse ro }

  /**
    * A fold across this (possibly asynchronous) collection
    * initial will only be evaluated in the success case.
    */
  override def foldLeft[B](initial: => B)(each: (B, T) => B): Ref[B] = {
    refs.iterator.foldLeft[Ref[B]](RefItself(initial)) { (soFar, ro) =>
      soFar.flatMapOpt { b =>
        ro map { t =>
          each(b, t)
        } orElse RefSome(b)
      }.require
    }
  }

  /**
    * Recovers from failures producing the list -- for instance if this is a RefFailed, or a RefFutureRefMany that fails.
    * Note that it does not recover from individual elements within the list failing.
    */
  override def recoverManyWith[B >: T](pf: PartialFunction[Throwable, RefMany[B]]): RefMany[B] = this

  /**
    * Called when the RefMany is "ready". This is equivalent to
    * fold(initial){ (_,_) => initial } but without calling the empty folder for each value
    */
  override def whenReady[B](f: RefMany[T] => B): Ref[B] = RefItself(f(this))
}

case class RefIterableRefMany[T, C[TT] <: IterableOnce[TT]](refs: IterableOnce[RefMany[T]]) extends RefMany[T] {

  def first:RefOpt[T] = refs.iterator.foldLeft[RefOpt[T]](RefNone) { (opt, rm) => opt orElse rm.first }

  def map[B](f: T => B):RefMany[B] = {
    val result = refs.iterator.map(_.map(f))
    RefIterableRefMany(result)
  }

  def flatMapOne[B](f: T => Ref[B]):RefMany[B] = {
    val result = refs.iterator.map(_.flatMap(f))
    RefIterableRefMany(result)
  }

  override def flatMapOpt[B](func: T => RefOpt[B]): RefMany[B] = {
    val result = refs.iterator.map(_.flatMap(func))
    RefIterableRefMany(result)
  }

  def flatMapMany[B](f: T => RefMany[B]):RefMany[B] = {
    val result = refs.iterator.map(_.flatMap(f))
    RefIterableRefMany(result)
  }

  def foreach[U](f: T => U):Unit = { refs.iterator.foreach(_.foreach(f)) }

  def withFilter(p: T => Boolean) = RefIterableRefMany(refs.iterator map (_ withFilter p))
  
  override def foldLeft[B](initial: =>B)(each:(B, T) => B):Ref[B] = {
    refs.iterator.foldLeft[Ref[B]](RefItself(initial)){(ar, refMany) =>
      ar.flatMapOne(a => refMany.foldLeft(a)(each))
    }     
  }

  def whenReady[B](block: RefMany[T] => B):Ref[B] = RefItself(block(this))
  
  def recoverManyWith[B >: T](pf: PartialFunction[Throwable, RefMany[B]]):RefMany[B] = this


}
