package com.wbillingsley.handy

import com.wbillingsley.handy.reactivestreams.TakeWhileR

import scala.annotation.tailrec
import scala.language.{higherKinds, postfixOps}

/**
 * A reference to a collection of items.
 */
case class RefTraversableOnce[T, C[TT] <: TraversableOnce[TT]](items: C[T]) extends RefManySync[T] {
    
  override def first:RefOpt[T] = RefOpt(headOption)

  def headOption:Option[T] = items.toStream.headOption

  override def map[B](f: T => B):RefManySync[B] = {
    val result = for (item <- items) yield f(item)
    RefTraversableOnce(result)
  }

  override def flatMapMany[B](f: T => RefMany[B]) = RefTraversableRefMany(items map f)

  override def flatMapOpt[B](f: T => RefOpt[B]):RefMany[B] = RefTraversableRefOpt(items map f)

  override def flatMapOne[B](f: T => Ref[B]) = RefTraversableRef(items map f)

  override def foreach[U](f: T => U):Unit = { items.foreach(f) }

  override def recoverManyWith[B >: T](pf: PartialFunction[Throwable, RefMany[B]]):RefMany[B] = this

  override def withFilter(p: T => Boolean) = RefTraversableOnce(items withFilter p)
  
  override def foldLeft[B](initial: =>B)(each:(B, T) => B) = RefItself(items.foldLeft(initial)(each))
  
  override def whenReady[B](block: RefMany[T] => B):Ref[B] = RefItself(block(this))
  
  override def collect = RefItself(items.toSeq)

  override def toSeq: Ref[Seq[T]] = collect
}

case class RefTraversableRef[T, C[TT] <: TraversableOnce[TT]](refs: TraversableOnce[Ref[T]]) extends RefMany[T] {

  override def first:RefOpt[T] = RefOpt(refs.toTraversable.headOption).flatMapOne(identity)

  override def map[B](f: T => B):RefMany[B] = {
    val result = refs.map(_.map(f))
    RefTraversableRef(result)
  } 

  override def flatMapOne[B](f: T => Ref[B]):RefMany[B] = {
    val result = refs.map(_.flatMap(f))
    RefTraversableRef(result)
  }

  override def flatMapOpt[B](func: T => RefOpt[B]): RefMany[B] = {
    val result = refs.map(_.flatMap(func))
    RefTraversableRefOpt(result)
  }

  override def flatMapMany[B](f: T => RefMany[B]):RefMany[B] = {
    val result = refs.map(_.flatMap(f))
    RefTraversableRefMany(result)
  }

  override def foreach[U](f: T => U):Unit = { refs.foreach(_.foreach(f)) }
  
  override def withFilter(p: T => Boolean):RefMany[T] = {
    flatMapOpt(x => if (p(x)) RefSome(x) else RefNone)
  }
  
  override def foldLeft[B](initial: =>B)(each:(B, T) => B):Ref[B] = {

    refs.foldLeft[Ref[B]](RefItself(initial)){(ar, br) =>
      for (a <- ar; ob <- br) yield each(a, ob)
    }     
  }
  
  override def whenReady[B](block: RefMany[T] => B):Ref[B] = RefItself(block(this))
  
  override def recoverManyWith[B >: T](pf: PartialFunction[Throwable, RefMany[B]]):RefMany[T] = this

}

case class RefTraversableRefOpt[+T](refs: TraversableOnce[RefOpt[T]]) extends RefMany[T] {

  /** map, as in functors */
  override def map[B](func: T => B): RefMany[B] = {
    RefTraversableRefOpt(refs.map(_.map(func)))
  }

  override def foreach[U](func: T => U): Unit = map(func)

  override def flatMapOne[B](func: T => Ref[B]): RefMany[B] = {
    RefTraversableRefOpt(refs.map(_.flatMapOne(func)))
  }

  override def flatMapOpt[B](func: T => RefOpt[B]): RefMany[B] = {
    RefTraversableRefOpt(refs.map(_.flatMapOpt(func)))
  }

  override def flatMapMany[B](func: T => RefMany[B]): RefMany[B] = {
    RefTraversableRefMany(refs.map(_.flatMapMany(func)))
  }

  override def withFilter(func: T => Boolean): RefMany[T] = {
    RefTraversableRefOpt(refs.map(_.withFilter(func)))
  }

  /** TODO: this consumes the whole stream */
  override def first: RefOpt[T] = refs.foldLeft[RefOpt[T]](RefNone) { (opt, ro) => opt orElse ro }

  /**
    * A fold across this (possibly asynchronous) collection
    * initial will only be evaluated in the success case.
    */
  override def foldLeft[B](initial: => B)(each: (B, T) => B): Ref[B] = {
    refs.foldLeft[Ref[B]](RefItself(initial)) { (soFar, ro) =>
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

case class RefTraversableRefMany[T, C[TT] <: TraversableOnce[TT]](refs: TraversableOnce[RefMany[T]]) extends RefMany[T] {

  def first:RefOpt[T] = refs.foldLeft[RefOpt[T]](RefNone) { (opt, rm) => opt orElse rm.first }

  def map[B](f: T => B):RefMany[B] = {
    val result = refs.map(_.map(f))
    RefTraversableRefMany(result)
  } 

  def flatMapOne[B](f: T => Ref[B]):RefMany[B] = {
    val result = refs.map(_.flatMap(f))
    RefTraversableRefMany(result)
  }

  override def flatMapOpt[B](func: T => RefOpt[B]): RefMany[B] = {
    val result = refs.map(_.flatMap(func))
    RefTraversableRefMany(result)
  }

  def flatMapMany[B](f: T => RefMany[B]):RefMany[B] = {
    val result = refs.map(_.flatMap(f))
    RefTraversableRefMany(result)
  }

  def foreach[U](f: T => U):Unit = { refs.foreach(_.foreach(f)) }
  
  def withFilter(p: T => Boolean) = RefTraversableRefMany(refs map (_ withFilter p))
  
  override def foldLeft[B](initial: =>B)(each:(B, T) => B):Ref[B] = {
    refs.foldLeft[Ref[B]](RefItself(initial)){(ar, refMany) =>
      ar.flatMapOne(a => refMany.foldLeft(a)(each))
    }     
  }

  def whenReady[B](block: RefMany[T] => B):Ref[B] = RefItself(block(this))
  
  def recoverManyWith[B >: T](pf: PartialFunction[Throwable, RefMany[B]]):RefMany[B] = this


}
