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

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds
import java.util.NoSuchElementException

/**
 * RefCanFlatMapTo. The flatMap method is designed to take an implicit
 * RCFMT. This allows us to determine the return type of flatMap (and
 * thus for loops) depending on the parameter type. See the implicit
 * objets in the Ref ccompanion object.
 */
trait RCFMT[-From[_], -To[_], +Result[_]] {  
  def flatMap[A, B](from: From[A], f: A => To[B]):Result[B]  
}

trait RMCFMT[-From[_], -To[_], +Result[_]] {  
  def flatMap[A, B](from: From[A], f: A => To[B]):Result[B]  
}

trait RSuper[+T] {
  type Repr[T] <: RSuper[T]
}

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
  
  def fromOptionId[T, K](opt: Option[K])(implicit lookUp:LookUpOne[T, K]) = {
    opt match {
      case Some(id) => new LazyId(id, lookUp)
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
   * FlatMap from one to option returns a singular Ref.
   */
  implicit object OneToOption extends RCFMT[Ref, Option, Ref] {    
    def flatMap[A, B](from: Ref[A], f: A => Option[B]) = {
      from.flatMapOne(item => Ref(f(item)))      
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
    def toRef(implicit ec:ExecutionContext) = new RefFuture(underlying)(ec)
  }  
  
  implicit class optToRef[T](val underlying: Option[T]) extends AnyVal {
	def toRef = fromOptionItem(underlying) 
  } 

  implicit class tryToRef[T](val underlying: scala.util.Try[T]) extends AnyVal {
    import scala.util.{Success, Failure}
    
    def toRef = underlying match {
      case Success(item) => item.itself
      case Failure(f) => RefFailed(f)
    }
  } 
  
  
  /**
   * flattens a RefMany[A[B]] by flatMaping itself to its contents
   */
  implicit class flattenableMany[B, A[B]](val underlying: RefMany[A[B]]) extends AnyVal {
    def flatten[Result[B]](implicit imp: RMCFMT[RefMany, A, Result]) = imp.flatMap(underlying, {a:A[B] => a})  
  }

  /**
   * flattens a Ref[A[B]] by flatMaping itself to its contents
   */
  implicit class flattenable[B, A[B]](val underlying: Ref[A[B]]) extends AnyVal {
    def flatten[Result[B]](implicit imp: RCFMT[Ref, A, Result]) = imp.flatMap(underlying, {a:A[B] => a})  
  }
  
  implicit def throwableToRef(exc: Throwable) = RefFailed(exc) 
  
  def itself[T](item: T) = RefItself(item)
    
  
  def apply[T, K](clazz : scala.Predef.Class[T], opt: Option[K])(implicit lookUp:LookUpOne[T, K]) = fromOptionId(opt)(lookUp)

  def apply[T](opt: Option[T]) = fromOptionItem(opt)
  
  /**
   * Converts a Ref[T] to a Ref[Option[T]]; by default RefFailed becomes None.itself, however 
   * that can be changed by passing a different recoverWith function.
   */
  def optionally[T](r:Ref[T], recoverWith: PartialFunction[Throwable, Ref[T]] = PartialFunction.apply { x:Throwable => RefNone }):Ref[Option[T]] = {
    r recoverWith(recoverWith) map(Some(_)) orIfNone None.itself
  }
  
}


/**
 * A reference to one item. This might be the item itself, a reference by ID, a Future from a fetch
 * operation, no item, a failure, etc.
 * 
 * There is a similar trait, RefMany, for dealing with references to multiple items.
 */
trait Ref[+T] extends RSuper[T] { 
  
  def fetch: ResolvedRef[T]

  /**
   * Returns an ID for the object only if one is immediately available (ie, not for incomplete futures)
   */
  def immediateId[TT >: T, K](implicit g:GetsId[TT, K]):Option[K]

  /**
   * Note that the way this is defined, you can flatMap from a RefOne to a RefMany.
   */
  def flatMap[B, R[B], Result[_]](func: T => R[B])(implicit imp: RCFMT[Ref, R, Result]):Result[B] = imp.flatMap(this, func)
  
  def flatMapOne[B](func: T => Ref[B]):Ref[B] 

  def flatMapMany[B](func: T => RefMany[B]):RefMany[B] 

  def withFilter(func: T => Boolean):Ref[T]
  
  def orIfNone[B >: T](f: => Ref[B]):Ref[B]
  
  def recoverWith[B >: T](pf: PartialFunction[Throwable, Ref[B]]):Ref[B]
  
  def map[B](func: T => B):Ref[B]

  def foreach[U](func: T => U):Unit 
  
  def onComplete[U](onSuccess: T => U, onNone: => U, onFail: Throwable => U):Unit  

  def refId[TT >: T, K](implicit g:GetsId[TT, K]):Ref[K]

  def toFuture = {
    import scala.concurrent._

    val p = Promise[T]()
    this onComplete(
      onSuccess = p success _,
      onNone = p failure new NoSuchElementException(),
      onFail = p failure _
      )
    p.future
  }

  /**
   * Converts this Ref to a Future[Option[T]] that might or might not immediately complete.
   */
  def toFutOpt:Future[Option[T]] = {
    import scala.concurrent._
    
    val p = Promise[Option[T]]()
      this onComplete(
        onSuccess = p success Some(_),        
        onNone = p success None,
        onFail = p failure _
      )
      p.future
  }
}

/**
 * A {@code Ref} that has an ID that can immediately be got.
 */
trait IdImmediate[+T] {
  def getId[TT >: T, K](implicit g:GetsId[TT, K]):Option[K]
}

/**
 * A resolved reference to an (or no) item
 */
trait ResolvedRef[+T] extends Ref[T] with TraversableOnce[T] with IdImmediate[T] {
  def fetch = this

  def toOption:Option[T]

  def toEither:Either[Throwable, Option[T]]
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

