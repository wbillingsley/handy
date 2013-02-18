package com.wbillingsley.handy

import scala.language.{higherKinds, postfixOps}

/**
 * A reference to a collection of items.
 */
case class RefTraversableOnce[T, C[T] <: TraversableOnce[T]](val items: C[T]) extends ResolvedRefMany[T] {
    
  def first = Ref.fromOptionItem(headOption)
  
  def headOption = items.toStream.headOption
  
  def toOption = headOption
  
  def isEmpty = items.isEmpty
  
  def map[B](f: T => B) = {
    val result = for (item <- items) yield f(item)
    RefTraversableOnce(result)
  } 
  
  def flatMapMany[B](f: T => RefMany[B]) = new RefTraversableRefMany(items map f)

  def flatMapOne[B](f: T => Ref[B]) = new RefTraversableRef(items map f)  
  
  def foreach[U](f: T => U) { items.foreach(f) }
  
  def isTraversableAgain = items.isTraversableAgain
  
  def toIterator = items.toIterator
  
  def toStream = items.toStream
  
  def copyToArray[B >: T](xs:Array[B], start:Int, len:Int) { items.copyToArray(xs, start, len) }
  
  def exists(p: T => Boolean) = items.exists(p)
  
  def find(p: T => Boolean) = items.find(p)
  
  def forall(p: T => Boolean) = items.forall(p)
  
  def hasDefiniteSize = items.hasDefiniteSize
  
  def seq = items.seq
  
  def toTraversable = items.toTraversable
  
  def withFilter(p: T => Boolean) = new RefTraversableOnce(items withFilter p)
  
  def fold[B](initial: =>B)(each:(B, T) => B) = RefItself(items.foldLeft(initial)(each))
  
  override def onReady[U](onSuccess: RefMany[T] => U, onNone: => U, onFail: Throwable => U) {
    if(!items.isEmpty) onSuccess(this) else onNone
  }
  
  override def toRefOne = RefItself(items)
  
}

case class RefTraversableRef[T, C[T] <: TraversableOnce[T]](val refs: TraversableOnce[Ref[T]]) extends RefMany[T] {
  
  def fetch = RefTraversableOnce(refs.flatMap(_.fetch))
  
  def first = Ref.fromOptionItem(headOption)
  
  def headOption = refs.flatMap(_.fetch).toStream.headOption
  
  def toOption = headOption
  
  def isEmpty = refs.forall(_.isEmpty)
  
  def map[B](f: T => B) = {
    val result = refs.map(_.map(f))
    RefTraversableRef(result)
  } 

  def flatMapOne[B](f: T => Ref[B]) = {
    val result = refs.map(_.flatMap(f))
    RefTraversableRef(result)
  }
  
  def flatMapMany[B](f: T => RefMany[B]) = {
    val result = refs.map(_.flatMap(f))
    RefTraversableRefMany(result)
  }

  def foreach[U](f: T => U) { refs.foreach(_.foreach(f)) }
  
  def withFilter(p: T => Boolean) = new RefTraversableRef(refs map (_ withFilter p))  
  
  def fold[B](initial: =>B)(each:(B, T) => B) = {
    import Ref._
    
    refs.foldLeft[Ref[B]](RefItself(initial)){(ar, br) =>
      for (a <- ar; ob <- optionally(br)) yield { 
	      ob match {
	        case Some(b) => each(a,b)
	        case None => a
	      }
      }
    }     
  }
  
  override def onReady[U](onSuccess: RefMany[T] => U, onNone: => U, onFail: Throwable => U) {
    if(!refs.isEmpty) onSuccess(this) else onNone
  }
  
  
}

case class RefTraversableRefMany[T, C[T] <: TraversableOnce[T]](val refs: TraversableOnce[RefMany[T]]) extends RefMany[T] {

  def fetch = RefTraversableOnce(refs.flatMap(_.fetch))
  
  def first = Ref.fromOptionItem(headOption)
  
  def headOption = refs.flatMap(_.fetch).toStream.headOption
  
  def toOption = headOption
  
  def isEmpty = refs.forall(_.isEmpty)
  
  def map[B](f: T => B) = {
    val result = refs.map(_.map(f))
    RefTraversableRefMany(result)
  } 

  def flatMapOne[B](f: T => Ref[B]) = {
    val result = refs.map(_.flatMap(f))
    RefTraversableRefMany(result)
  }
  
  def flatMapMany[B](f: T => RefMany[B]) = {
    val result = refs.map(_.flatMap(f))
    RefTraversableRefMany(result)
  }

  def foreach[U](f: T => U) { refs.foreach(_.foreach(f)) }
  
  def withFilter(p: T => Boolean) = new RefTraversableRefMany(refs map (_ withFilter p)) 
  
  def fold[B](initial: =>B)(each:(B, T) => B) = {
    refs.foldLeft[Ref[B]](RefItself(initial)){(ar, refMany) =>
      ar.flatMap(a => refMany.fold(a)(each))
    }     
  }  
  
  override def onReady[U](onSuccess: RefMany[T] => U, onNone: => U, onFail: Throwable => U) {
    if(!refs.isEmpty) onSuccess(this) else onNone
  }
  
  
}
