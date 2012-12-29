/*
This file is available under the MIT licence:

Copyright (C) 2012 William Billingsley

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
  documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit
  persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
  Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
  OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
package com.wbillingsley.handy

import scala.language.higherKinds


/**
 * Companion object
 */
object Ref {

  import scala.language.implicitConversions

  def fromOptionId[T, K](clazz : scala.Predef.Class[T], opt: Option[K]):Ref[T] = {
    opt match {
      case Some(id) => RefById(clazz, id)
      case None => RefNone
    }
  }

  implicit def fromOptionItem[T](opt: Option[T]):ResolvedRef[T] = {
    opt match {
      case Some(item) => RefItself(item)
      case None => RefNone
    }
  }
  
  def itself[T](item: T) = RefItself(item)
  
  def apply[T, K](clazz : scala.Predef.Class[T], opt: Option[K]) = fromOptionId(clazz, opt)

  def apply[T](opt: Option[T]) = fromOptionItem(opt)
}


/**
 * A generic reference to something.  Used so we can talk about something (usually a persisted object) without having
 * to fetch it first.
 */
trait Ref[+T] {
  
  def fetch: ResolvedRef[T]

  def toOption:Option[T] 
  
  def toEither:Either[Throwable, Option[T]] = fetch.toEither
    
  /**
   * Note that the way this is defined, you can flatMap from a RefOne to a RefMany.
   * 
   * We use the higher kinded type so that subclasses can narrow the return type to R[B] if
   * they choose.  For instance, RefItself can (as it can just return the function result),
   * but a RefFuture could not (as if it's called with a function returning a ResolvedRef, the 
   * result must nonetheless be a RefFuture).  Neither can RefTraversableOnce.
   */
  def flatMap[B, R[B] >: RefNothing <: Ref[B]](func: T => R[B]):Ref[B] 
  
  def map[B](func: T => B):Ref[B]

  def foreach[U](func: T => U):Unit 

  def isEmpty: Boolean
  
  def orIfNone[B >: T](f: => Ref[B]) = {
    if (isEmpty) f else this
  }
  
  def getId[TT >: T, K](implicit g:GetsId[TT, K]):Option[K] 
  
  def sameId[TT >: T](other: Ref[TT])(implicit g:GetsId[TT, _]) = getId(g) == other.getId(g)
}

/**
 * A reference that is expected to contain only a single item
 */
trait RefOne[+T] extends Ref[T] {
  def map[B](func: T => B):RefOne[B]

  override def fetch: ResolvedRef[T] with RefOne[T]	  
}

/**
 * A reference that is expected to contain several items
 */
trait RefMany[+T] extends Ref[T] {
  override def map[B](func: T => B):RefMany[B]  
}

/**
 * A resolved reference to an (or no) item
 */
trait ResolvedRef[+T] extends Ref[T] with TraversableOnce[T] {
  def fetch = this  
}

/**
 * A reference that has not yet been looked up
 */
trait UnresolvedRef[+T] extends Ref[T] 


/**
 * A reference that has nothing at the end of it, either through being a failed reference or the empty reference
 */
trait RefNothing extends ResolvedRef[Nothing] with RefOne[Nothing] with RefMany[Nothing] {
  
  override def getId[TT >: Nothing, K](implicit g:GetsId[TT, K]) = None
  
  def isEmpty = true
  def toOption = None
  def getId = None
  
  override def fetch = this
  
  def foreach[U](f: Nothing => U) { /* does nothing */ }
  
  def map[B](f: Nothing => B) = this  
  
  def flatMap[B, R[B] >: RefNothing](f: Nothing => R[B]) = this
      
  def isTraversableAgain = true
  
  def toIterator = Iterator.empty
  
  def toStream = Stream.empty
  
  def copyToArray[B >: Nothing](xs:Array[B], start:Int, len:Int) { /* nothing to copy */ }
  
  def exists(p: Nothing => Boolean) = false
  
  def find(p: Nothing => Boolean) = None
  
  def forall(p: Nothing => Boolean) = Iterator.empty.forall(p)
  
  def hasDefiniteSize = true
  
  def seq = Iterator.empty.seq
  
  def toTraversable = Seq.empty  
  
  
}



/**
 * A failure to find a reference
 * @param msg description of the failure
 * @param exception an exception if there was one
 */
case class RefFailed(exception: Throwable) extends RefNothing {

  override def toEither = Left(exception)
    
}

/**
 * Singleton to say there's nothing there.
 */
case object RefNone extends RefNothing {
  
  override def toEither = Right(None)
  
}

/**
 * A reference to an item that has been fetched.
 */
case class RefItself[T](item: T) extends ResolvedRef[T] with RefOne[T] {
  
  def toOption = Some(item)
  
  override def toEither = Right(Some(item))
  
  def isEmpty = false
  
  def count = 1
  
  def map[B](f: T => B) = {
    val x = f(item)
    if (x == null) RefNone else RefItself(x)
  } 
  
  def flatMap[B, R[B] >: RefNothing <: Ref[B]](f: T => R[B]):R[B] = f(item)
  
  def foreach[U](f: T => U) { f(item) }
  
  override def getId[TT >: T, K](implicit g:GetsId[TT, K]) = g.getId(item)
  
  def isTraversableAgain = true
  
  def toIterator = Iterator(item)
  
  def toStream = Stream(item)
  
  def copyToArray[B >: T](xs:Array[B], start:Int, len:Int) { 
    toIterator.copyToArray(xs, start, len) 
  }
  
  def exists(p: T => Boolean) = p(item)
  
  def find(p: T => Boolean) = if (p(item)) Some(item) else None
  
  def forall(p: T => Boolean) = p(item)
  
  def hasDefiniteSize = true
  
  def seq = toOption
  
  def toTraversable = toOption  
  
  override def fetch = this
    
  
}

