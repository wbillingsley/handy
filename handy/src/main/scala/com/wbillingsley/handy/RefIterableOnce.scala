package com.wbillingsley.handy

import scala.annotation.tailrec
import scala.language.{higherKinds, postfixOps}

/**
 * A reference to a collection of items.
 */
case class RefIterableOnce[T, C <: IterableOnce[T]](items: C) extends RefManySync[T] {

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

  class RIterator(it:Iterator[T], thisItem:T) extends RefIterator[T] {
    override def item: RefOpt[T] = RefSome(thisItem)
    override def next: RefOpt[RefIterator[T]] = for item <- it.nextOption().toRefOpt yield RIterator(it, item)
  }

  override def iterator: RefOpt[RefIterator[T]] = {
    val it = items.iterator
    for
    item <- it.nextOption().toRefOpt
      yield RIterator(it, item)
  }
}

case class RefIterableRef[T, C <: IterableOnce[Ref[T]]](refs: C) extends RefMany[T] {

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


  class RIterator(it:Iterator[Ref[T]], thisItem:Ref[T]) extends RefIterator[T] {
    override def item: RefOpt[T] = thisItem.toRefOpt
    override def next: RefOpt[RefIterator[T]] = for item <- it.nextOption().toRefOpt yield RIterator(it, item)
  }

  override def iterator: RefOpt[RefIterator[T]] = {
    val it = refs.iterator
    for
      item <- it.nextOption().toRefOpt
    yield RIterator(it, item)
  }
  
}

case class RefIterableRefOpt[T, C <: IterableOnce[RefOpt[T]]](refs: C) extends RefMany[T] {

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


  class RIterator(inner:Iterator[RefOpt[T]]) extends RefIterator[T] {
    private val no = inner.nextOption()
    override def item: RefOpt[T] = no.toRefOpt.flatMapOpt(identity)
    override def next: RefOpt[RefIterator[T]] = (no.map(_ => new RIterator(inner))).toRefOpt
  }

  override def iterator: RefOpt[RefIterator[T]] = RefSome(new RIterator(refs.iterator))
}

case class RefIterableRefMany[T, C <: IterableOnce[RefMany[T]]](refs: C) extends RefMany[T] {

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

  class RIterator(rms:Iterator[RefMany[T]], delegate:RefIterator[T]) extends RefIterator[T] {
    override def item: RefOpt[T] = delegate.item
    
    override def next: RefOpt[RefIterator[T]] = {
      delegate.next.option.flatMap {
        case Some(nextDel) => RefSome(RIterator(rms, nextDel))
        case None =>
          (for
            next <- rms.nextOption().toRefOpt
            del <- next.iterator
          yield RIterator(rms, del))
      }
    }
  }

  override def iterator: RefOpt[RefIterator[T]] = { 
    val it = refs.iterator
    for 
      rm <- it.nextOption().toRefOpt
      del <- rm.iterator
    yield RIterator(it, del)
  }
}
