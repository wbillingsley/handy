package com.wbillingsley.handy

import scala.language.higherKinds

/**
 * A reference to a collection of items.
 */
case class RefTraversableOnce[T, C[T] <: TraversableOnce[T]](val items: C[T]) extends ResolvedRef[T] with RefMany[T] {
  
  override def getId[TT >: T, KK](implicit g:GetsId[TT, KK]) = Ref.fromOptionItem(headOption).getId(g)
  
  def first = Ref.fromOptionItem(headOption)
  
  def headOption = items.toStream.headOption
  
  def toOption = headOption
  
  def isEmpty = items.isEmpty
  
  def map[B](f: T => B) = {
    val result = for (item <- items) yield f(item)
    RefTraversableOnce(result)
  } 

  def flatMap[B, R[B] >: RefNothing <: Ref[B]](f: T => R[B]) = {        
    val results = for (item <- items) yield f(item)
    if (results.isEmpty) { RefNone } else { RefTraversableRef(results) }
  }
  
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
  
}

case class RefTraversableRef[T, C[T] <: TraversableOnce[T]](val refs: TraversableOnce[Ref[T]]) extends Ref[T] with RefMany[T] {

  override def getId[TT >: T, KK](implicit g:GetsId[TT, KK]) = Ref.fromOptionItem(headOption).getId(g)
  
  def fetch = RefTraversableOnce(refs.flatMap(_.fetch))
  
  def first = Ref.fromOptionItem(headOption)
  
  def headOption = refs.flatMap(_.fetch).toStream.headOption
  
  def toOption = headOption
  
  def isEmpty = refs.forall(_.isEmpty)
  
  def map[B](f: T => B) = {
    val result = refs.map(_.map(f))
    RefTraversableRef(result)
  } 

  def flatMap[B, R[B] >: RefNothing <: Ref[B]](f: T => R[B]) = {
    val result = refs.map(_.flatMap(f))
    RefTraversableRef(result)
  }
  
  def foreach[U](f: T => U) { refs.foreach(_.foreach(f)) }
  
}

