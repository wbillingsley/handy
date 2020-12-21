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
import scala.util.{Failure, Success, Try}

/**
 * RefCanFlatMapTo. The flatMap method is designed to take an implicit
 * RCFMT. This allows us to determine the return type of flatMap (and
 * thus for loops) depending on the parameter type. See the implicit
 * objets in the Ref ccompanion object.
 */
trait RCFMT[-From[AA], -To[BB], +Result[BB]] {
  def flatMap[A, B](from: From[A], f: A => To[B]):Result[B]  
}


trait RMCFMT[-From[AA], -To[BB], +Result[BB]] {
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

  def fromOption[T](opt: Option[T]):RefOpt[T] = RefOpt.apply(opt)

  def fromFuture[T](fut: Future[T])(implicit ec:ExecutionContext):RefFuture[T] = {
    new RefFuture(fut)(ec)
  }

  /**
   * FlatMap from one to one returns a singular Ref.
   */
  implicit object OneToOne extends RCFMT[Ref, Ref, Ref] {
    def flatMap[A, B](from: Ref[A], f: A => Ref[B]):Ref[B] = {
      from.flatMapOne(f)
    }
  }

  /**
   * FlatMap from one to option returns a singular Ref.
   */
  implicit object OneToOpt extends RCFMT[Ref, RefOpt, RefOpt] {
    def flatMap[A, B](from: Ref[A], f: A => RefOpt[B]):RefOpt[B] = {
      from.flatMapOpt(f)
    }
  }

  /**
   * FlatMap from one to many returns a RefMany.
   */
  implicit object OneToMany extends RCFMT[Ref, RefMany, RefMany] {
    def flatMap[A, B](from: Ref[A], f: A => RefMany[B]):RefMany[B] = {
      from.flatMapMany(f)
    }
  }

  /**
   * Adds a toRef method to convert this collection into a RefMany.  Note that we do not just
   * implicitly convert from TraversableOnce to Ref because it would cause there to be a
   * compiler error with an ambiguous conversion for some collections with flatMap
   * (as both a conversion to Ref and a conversion to MonadOps could provide flatMap)
   */
  implicit class travToRef[T, C[TT] <: IterableOnce[TT]](val underlying: C[T]) extends AnyVal {
    def toRefMany = RefIterableOnce(underlying)
  }

  implicit class futToRef[T](val underlying: Future[T]) extends AnyVal {
    def toRef(implicit ec:ExecutionContext) = new RefFuture(underlying)(ec)
  }

  implicit class futOptToRefOpt[T](val underlying: Future[Option[T]]) extends AnyVal {
    def toRefOpt(implicit ec:ExecutionContext) = RefOpt.fromFutureOpt(underlying)(ec)
  }

  implicit class optToRef[T](val underlying: Option[T]) extends AnyVal {
    def toRef:RefOpt[T] = RefOpt.apply(underlying)
  }

  implicit class tryToRef[T](val underlying: scala.util.Try[T]) extends AnyVal {
    import scala.util.{Success, Failure}

    def toRef:Ref[T] = underlying match {
      case Success(item) => RefItself(item)
      case Failure(f) => RefFailed(f)
    }
  }

  implicit def throwableToRef(exc: Throwable):RefFailed = RefFailed(exc)

  def itself[T](item: T) = RefItself(item)

  def pure[T](item: T) = RefItself(item)

  def apply[T](tr:Try[T]):RefSync[T] = tr match {
    case Success(t) => RefItself(t)
    case Failure(f) => RefFailed(f)
  }

  /*
   * These wire flatMap to the correct version by overloading.
   * Function[T,R] gets type-erased, so we have to compel the compiler to put a different object with a different type
   * into the second argument list in order to ensure that flatMap overloads correctly.
   */
  implicit object FMOne
  implicit object FMOpt
  implicit object FMMany

}


/**
 * A reference to one item. This might be the item itself, a reference by ID, a Future from a fetch
 * operation, no item, a failure, etc.
 * 
 * There is a similar trait, RefMany, for dealing with references to multiple items.
 */
trait Ref[+T] extends RSuper[T] { 
  
  /**
   * Returns an ID for the object only if one is immediately available (ie, not for incomplete futures)
   */
  def immediateId[TT >: T, Key <: Id[TT, _]](implicit g:GetsId[TT, Key]):Option[Key] = None

  /**
   * Allows use in for comprehensions. Automatically routes to flatMapOne (bind), flatMapOpt, or flatMapMany
   */
  def flatMap[B, R[_], Result[_]](func: T => R[B])(implicit imp: RCFMT[Ref, R, Result]):Result[B] = {
    imp.flatMap(this, func)
  }

  /** bind, as in monads */
  def bind[B](func: T => Ref[B]):Ref[B] = flatMapOne(func)

  def flatMapOne[B](func: T => Ref[B]):Ref[B]

  /**
    * Allows use in for comprehensions. Automatically routes to flatMapOne (bind)
    */
  def flatMap[B](func: T => Ref[B])(implicit ev:Ref.FMOne.type):Ref[B] = flatMapOne(func)

  def flatMapOpt[B](func: T => RefOpt[B]):RefOpt[B]

  /**
    * Allows use in for comprehensions. Automatically routes to flatMapOpt
    */
  def flatMap[B](func: T => RefOpt[B])(implicit ev:Ref.FMOpt.type):RefOpt[B] = flatMapOpt(func)

  def flatMapMany[B](func: T => RefMany[B]):RefMany[B]

  /**
    * Allows use in for comprehensions. Automatically routes to flatMapMany
    */
  def flatMap[B](func: T => RefMany[B])(implicit ev:Ref.FMMany.type):RefMany[B] = flatMapMany(func)

  def recoverWith[B >: T](pf: PartialFunction[Throwable, Ref[B]]):Ref[B]
  
  def map[B](func: T => B):Ref[B]

  def foreach[U](func: T => U):Unit 

  /** Enables 'if' and 'match' in for comprehensions. NB, we're being a bit tricky because the return type changes. */
  def withFilter(pred: T => Boolean):RefOpt[T] = toRefOpt.withFilter(pred)
  
  def refId[TT >: T, K <: Id[TT, _]](implicit g:GetsId[TT, K]):RefOpt[K] = {
    RefOpt.apply(immediateId(g)) orElse {
      this.flatMapOpt(i => RefOpt.apply(g.getId(i)))
    }
  }

  def toFuture:Future[T]

  def toRefOpt:RefOpt[T] = this.flatMapOpt(RefSome.apply) recoverWith {
    case _:NoSuchElementException => RefNone
  }

  def option:Ref[Option[T]] = toRefOpt.option

}

/**
 * Equivalent to Try[T]
 */
trait RefSync[+T] extends Ref[T] {
  def toEither:Either[Throwable, T]

  def toTry:Try[T]
}

