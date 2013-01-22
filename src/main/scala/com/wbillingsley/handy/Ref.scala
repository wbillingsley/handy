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

import scala.concurrent.Future
import scala.language.higherKinds

/**
 * RefCanFlatMapTo. The flatMap method is designed to take an implicit
 * RCFMT. This allows us to determine the return type of flatMap (and
 * thus for loops) depending on the parameter type. See the implicit
 * objets in the Ref ccompanion object.
 */
trait RCFMT[-From[_], -To[_], +Result[_]] {  
  def flatMap[A, B](from: From[A], f: A => To[B]):Result[B]  
}


trait RSuper[+T]

/**
 * Companion object
 */
object Ref {

  import scala.language.implicitConversions

  /**
   * Allows us to say "foo itself" and get a RefItself(foo)
   */
  implicit class Itself[T](val it:T) extends AnyVal { 
    def itself = RefItself(it) 
  }
  
  def fromOptionId[T, K](clazz : scala.Predef.Class[T], opt: Option[K]) = {
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
  
  /** 
   * FlatMap from one to one returns a singular Ref.
   */
  implicit object OneToOne extends RCFMT[Ref, Ref, Ref] {    
    def flatMap[A, B](from: Ref[A], f: A => Ref[B]) = {
      from.flatMapOne(f)
    }    
  }
  
  /** 
   * FlatMap from one to many returns a RefMany.
   */
  implicit object OneToMany extends RCFMT[Ref, RefMany, RefMany] {    
    def flatMap[A, B](from: Ref[A], f: A => RefMany[B]) = {
      from.flatMapMany(f)
    }    
  }
    
  /**
   * Adds a toRef method to convert this collection into a RefMany.  Note that we do not just
   * implicitly convert from TraversableOnce to Ref because it would cause there to be a 
   * compiler error with an ambiguous conversion for some collections with flatMap
   * (as both a conversion to Ref and a conversion to MonadOps could provide flatMap)
   */
  implicit class travToRef[T, C[T] <: TraversableOnce[T]](val underlying: C[T]) extends AnyVal {
	def toRefMany = new RefTraversableOnce(underlying)  
  }  
  
  implicit class futToRef[T](val underlying: Future[T]) extends AnyVal {
	def toRef = new RefFuture(underlying)  
  }  
  
  
  def itself[T](item: T) = RefItself(item)
    
  
  def apply[T, K](clazz : scala.Predef.Class[T], opt: Option[K]) = fromOptionId(clazz, opt)

  implicit def apply[T](opt: Option[T]) = fromOptionItem(opt)
}


/**
 * A reference to one item. This might be the item itself, a reference by ID, a Future from a fetch
 * operation, no item, a failure, etc.
 * 
 * There is a similar trait, RefMany, for dealing with references to multiple items.
 */
trait Ref[+T] extends RSuper[T] {
  
  def fetch: ResolvedRef[T]

  def toOption:Option[T] 
  
  def toEither:Either[Throwable, Option[T]] = fetch.toEither
  
  /**
   * Note that the way this is defined, you can flatMap from a RefOne to a RefMany.
   */
  def flatMap[B, R[B], Result[B]](func: T => R[B])(implicit imp: RCFMT[Ref, R, Result]):Result[B] = imp.flatMap(this, func)
  
  def flatMapOne[B](func: T => Ref[B]):Ref[B] 

  def flatMapMany[B](func: T => RefMany[B]):RefMany[B] 

  def withFilter(func: T => Boolean):Ref[T]
  
  def orIfNone[B >: T](f: => Ref[B]):Ref[B] = {
    if (isEmpty) f else this
  }       
  
  def map[B](func: T => B):Ref[B]

  def foreach[U](func: T => U):Unit 
  
  def onComplete[U](onSuccess: T => U, onNone: => U, onFail: Throwable => U):Unit  

  def isEmpty: Boolean
  
  def getId[TT >: T, K](implicit g:GetsId[TT, K]):Option[K] 
  
  def sameId[TT >: T](other: Ref[TT])(implicit g:GetsId[TT, _]) = getId(g) == other.getId(g)
}





/**
 * A reference that is expected to contain several items
 */
trait RefMany[+T] extends RSuper[T] {
  
  def fetch:ResolvedRefMany[T]
  
  def map[B](func: T => B):RefMany[B]  
  
  def foreach[U](func: T => U):Unit 

  def isEmpty: Boolean

  def flatMap[B, R[B], Result[B]](func: T => R[B])(implicit imp: RCFMT[RefMany, R, Result]):Result[B] = imp.flatMap(this, func)

  def flatMapOne[B](func: T => Ref[B]):RefMany[B] 

  def flatMapMany[B](func: T => RefMany[B]):RefMany[B] 
  
  def withFilter(func: T => Boolean):RefMany[T]  

}

object RefMany {
  
  /** 
   * FlatMap from many to many returns a RefMany.
   */
  implicit object ManyToMany extends RCFMT[RefMany, RefMany, RefMany] {    
    def flatMap[A, B](from: RefMany[A], f: A => RefMany[B]) = {
      from.flatMapMany(f)
    }    
  }  
  
  
  /** 
   * FlatMap from many to one returns a RefMany.
   */
  implicit object ManyToOne extends RCFMT[RefMany, Ref, RefMany] {    
    def flatMap[A, B](from: RefMany[A], f: A => Ref[B]) = {
      from.flatMapOne(f)
    }    
  }  
}

/**
 * A resolved reference to an (or no) item
 */
trait ResolvedRef[+T] extends Ref[T] with TraversableOnce[T] {
  def fetch = this  
}

/**
 * A resolved reference to a number of items
 */
trait ResolvedRefMany[+T] extends RefMany[T] with TraversableOnce[T] {
  def fetch = this  
}


/**
 * A reference that has not yet been looked up
 */
trait UnresolvedRef[+T] extends Ref[T] 


/**
 * A reference that has nothing at the end of it, either through being a failed reference or the empty reference
 */
trait RefNothing extends ResolvedRef[Nothing] with ResolvedRefMany[Nothing] {
  
  override def getId[TT >: Nothing, K](implicit g:GetsId[TT, K]) = None
  
  def isEmpty = true
  def toOption = None
  def getId = None
  
  override def fetch = this
  
  def foreach[U](f: Nothing => U) { /* does nothing */ }
  
  def map[B](f: Nothing => B) = this  
  
  def flatMapOne[B](f: Nothing => Ref[B]) = this

  def flatMapMany[B](f: Nothing => RefMany[B]) = this
  
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
     
  def withFilter(p: Nothing => Boolean) = this  
  
}



/**
 * A failure to find a reference
 * @param msg description of the failure
 * @param exception an exception if there was one
 */
case class RefFailed(exception: Throwable) extends RefNothing {

  override def toEither = Left(exception)
       
  def onComplete[U](onSuccess: Nothing => U, onNone: => U, onFail: Throwable => U) { 
    onFail(exception)
  } 
}

/**
 * Singleton to say there's nothing there.
 */
case object RefNone extends RefNothing {
  
  override def toEither = Right(None)  
  
  def onComplete[U](onSuccess: Nothing => U, onNone: => U, onFail: Throwable => U) { 
    onNone
  }
   
}

/**
 * A reference to an item that has been fetched.
 */
case class RefItself[T](val item: T) extends ResolvedRef[T] {
  
  def toOption = Some(item)
  
  override def toEither = Right(Some(item))
  
  def isEmpty = false
  
  def count = 1
  
  def map[B](f: T => B) = {
    val x = f(item)
    if (x == null) RefNone else RefItself(x)
  } 
  
  def flatMapOne[B](f: T => Ref[B]):Ref[B] = f(item)

  def flatMapMany[B](f: T => RefMany[B]):RefMany[B] = f(item)

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
    
  def withFilter(p: T => Boolean) = if (p(item)) this else RefNone
 
  def onComplete[U](onSuccess: T => U, onNone: => U, onFail: Throwable => U) { 
    onSuccess(item)
  }  
  
}

