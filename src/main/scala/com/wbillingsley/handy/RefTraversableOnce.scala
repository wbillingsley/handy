package com.wbillingsley.handy

/**
 * A reference to a collection of items.
 */
case class RefTraversableOnce[T](items: TraversableOnce[T]) extends ResolvedRef[T] with RefMany[T] {
  
  def toOption = items.toStream.headOption
  
  def isEmpty = items.isEmpty
  
  def map[B](f: T => B) = {
    val result = for (item <- items) yield f(item)
    RefTraversableOnce(result)
  } 

  def flatMap[B](f: T => Ref[B]) = {
    val results = for (item <- items; result <- f(item)) yield {
      result
    }
    if (results.isEmpty) { RefNone } else { RefTraversableOnce(results) }
  }
  
  def foreach[U](f: T => U) { items.foreach(f) }
  
  def orIfNone[B >: T](f: => Ref[B]):Ref[B] = this  
  
  def getId = None
  
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