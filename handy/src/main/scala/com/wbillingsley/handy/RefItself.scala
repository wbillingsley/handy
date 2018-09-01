package com.wbillingsley.handy

import scala.concurrent.Future
import scala.util.Success

/**
 * A reference to an item that has been fetched.
 */
case class RefItself[T](val item: T) extends RefSync[T] {
  
  override def toEither = Right(item)

  override def toTry = Success(item)

  override def map[B](f: T => B) = RefItself(f(item))
  
  override def flatMapOne[B](f: T => Ref[B]):Ref[B] = f(item)

  override def flatMapOpt[B](f: T => RefOpt[B]):RefOpt[B] = f(item)

  override def flatMapMany[B](f: T => RefMany[B]):RefMany[B] = f(item)

  override def foreach[U](f: T => U) { f(item) }

  override def recoverWith[B >: T](pf: PartialFunction[Throwable, Ref[B]]) = this

  override def toFuture = Future.successful(item)

}