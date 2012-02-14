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

  def fromOptionItem[T](opt: Option[T]):Ref[T] = {
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
sealed abstract class Ref[+T] {
  def fetch: ResolvedRef[T]

  def toOption:Option[T]

  def flatMap[B](func: T => Ref[B]) = {
    fetch match {
      case RefItself(item) => func(item)
      case r:RefNothing => r
    }
  }
  
  def map[B](func: T => B) = {
    fetch match {
      case RefItself(item) => {
        val x = func(item)
        if (x == null) RefNone else RefItself(x)
      }
      case r:RefNothing => r
    }
  }
}

/**
 * A resolved reference to an (or no) item
 */
sealed abstract class ResolvedRef[+T] extends Ref[T] {
  def fetch = this

  def isEmpty:Boolean
}

/**
 * A reference that has not yet been looked up
 */
abstract class UnresolvedRef[+T] extends Ref[T] {
  def fetch = Ref.resolve(this)
}

/**
 * A reference that has nothing at the end of it, either through being a failed reference or the empty reference
 */
abstract class RefNothing extends ResolvedRef[Nothing] {
  def isEmpty = true
  def toOption = None
}

/**
 * A reference to an item by its id.
 */
case class RefById[T, K](clazz : scala.Predef.Class[T], id: K) extends UnresolvedRef[T] {
  def toOption = fetch.toOption
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
object RefNone extends RefNothing

/**
 * A reference to an item that has been fetched.
 */
case class RefItself[T](item: T) extends ResolvedRef[T] {
  def toOption = Some(item)
  def isEmpty = false
}
