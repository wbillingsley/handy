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

/**
 * Companion object
 */
object Ref {

  var resolver:Option[RefResolver] = None

  def resolve[T](unresolved:UnresolvedRef[T]) = {
    resolver match {
      case Some(r) => r.resolve(unresolved)
      case None => RefNone
    }
  }

  def fromOptionId[T, K](clazz : scala.Predef.Class[T], opt: Option[K]):Ref[T] = {
    opt match {
      case Some(id) => RefById(clazz, id)
      case None => RefNone
    }
  }

  def fromOptionItem[T](opt: Option[T]):ResolvedRef[T] = {
    opt match {
      case Some(item) => RefItself(item)
      case None => RefNone
    }
  }

  def apply[T, K](clazz : scala.Predef.Class[T], opt: Option[K]) = fromOptionId(clazz, opt)

  def apply[T](opt: Option[T]) = fromOptionItem(opt)
}

/**
 * Knows how to resolve references. Use this trait to supply a hook into your ORM or persistence
 * mechanism.
 */
trait RefResolver {
  def resolve[T](unresolved:UnresolvedRef[T]):ResolvedRef[T]
}

/**
 * A generic reference to something.  Used so we can talk about something (usually a persisted object) without having
 * to fetch it first.
 */
trait Ref[+T] extends TraversableOnce[T] {
  def fetch: ResolvedRef[T]

  def toOption:Option[T] 
  
  def flatMap[B](func: T => Ref[B]):Ref[B] 
  
  def map[B](func: T => B):Ref[B]

  def foreach[U](func: T => U):Unit 

  def orIfNone[B >: T](f: => Ref[B]):Ref[B]

  def getId:Option[Any]  
}

trait RefOne[+T] extends Ref[T]

trait RefMany[+T] extends Ref[T]

/**
 * A resolved reference to an (or no) item
 */
trait ResolvedRef[+T] extends Ref[T] {
  def fetch = this

  def isEmpty:Boolean
  
}

/**
 * A reference that has not yet been looked up
 */
trait UnresolvedRef[+T] extends Ref[T] {
  def fetch = Ref.resolve(this)
  
  def map[B](f: T => B) = fetch.map(f)
  
  def foreach[U](f: T => U) { fetch.foreach(f) }
  
  def flatMap[B](f: T => Ref[B]) = fetch.flatMap(f)
  
  def orIfNone[B >: T](f: => Ref[B]):Ref[B] = fetch.orIfNone(f)
}

/**
 * A reference that has nothing at the end of it, either through being a failed reference or the empty reference
 */
trait RefNothing extends ResolvedRef[Nothing] with RefOne[Nothing] with RefMany[Nothing] {
  def isEmpty = true
  def toOption = None
  def getId = None
  
  def foreach[U](f: Nothing => U) { /* does nothing */ }
  
  def map[B](f: Nothing => B) = this  
  
  def flatMap[B](f: Nothing => Ref[B]) = this
  
  def orIfNone[B >: Nothing](f: => Ref[B]):Ref[B] = f
    
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
 * A reference to an item by its id.
 */
case class RefById[T, K](clazz : scala.Predef.Class[T], id: K) extends UnresolvedRef[T] with RefOne[T] {
  def toOption = fetch.toOption
  def getId = Some(id)
  
  def isTraversableAgain = true
  
  def toIterator = fetch.toIterator
  
  def toStream = fetch.toStream
  
  def copyToArray[B >: T](xs:Array[B], start:Int, len:Int) { 
    fetch.copyToArray(xs, start, len) 
  }
  
  def exists(p: T => Boolean) = fetch.exists(p)
  
  def find(p: T => Boolean) = fetch.find(p)
  
  def forall(p: T => Boolean) = fetch.forall(p)
  
  def hasDefiniteSize = fetch.hasDefiniteSize
  
  def seq = fetch.seq
  
  def toTraversable = fetch.toTraversable   
  
  def isEmpty = fetch.isEmpty	
  
}

/**
 * A failure to find a reference
 * @param msg description of the failure
 * @param exception an exception if there was one
 */
case class RefFailed(msg: String, exception: Option[Throwable]) extends RefNothing

/**
 * Singleton to say there's nothing there.
 */
case object RefNone extends RefNothing

/**
 * A reference to an item that has been fetched.
 */
case class RefItself[T](item: T) extends ResolvedRef[T] with RefOne[T] {
  type hasId = { def id:Any }
  
  def toOption = Some(item)
  
  def isEmpty = false
  
  def count = 1
  
  def map[B](f: T => B) = {
    val x = f(item)
    if (x == null) RefNone else RefItself(x)
  } 

  def flatMap[B](f: T => Ref[B]) = f(item)
  
  def foreach[U](f: T => U) { f(item) }
  
  def orIfNone[B >: T](f: => Ref[B]):Ref[B] = this  
  
  def getId = {
    if (item.isInstanceOf[hasId]) {
      Some(item.asInstanceOf[hasId].id)
    } else None
  }
  
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
    
  
}

