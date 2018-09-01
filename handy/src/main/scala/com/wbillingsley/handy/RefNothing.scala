package com.wbillingsley.handy

import scala.concurrent.Future
import scala.util.{Failure, Try}


/**
 * A failure to find a reference
 *
 * @param exception an exception if there was one
 */
case class RefFailed(exception: Throwable) extends Ref[Nothing] with RefSync[Nothing] {

  override def toEither = Left(exception)
       
  override def toFuture = Future.failed(exception)

  override def recoverWith[B >: Nothing](pf: PartialFunction[Throwable, Ref[B]]) = pf.applyOrElse(exception, { x:Throwable => this })

  override def flatMapOne[B](func: Nothing => Ref[B]): Ref[B] = this

  override def flatMapOpt[B](func: Nothing => RefOpt[B]): RefOpt[B] = RefOptFailed(exception)

  override def flatMapMany[B](func: Nothing => RefMany[B]): RefMany[B] = RefManyFailed(exception)

  override def map[B](func: Nothing => B): Ref[B] = this

  override def foreach[U](func: Nothing => U): Unit = {}

  override def toTry: Try[Nothing] = Failure(exception)

}

object RefFailed {
  
  import scala.language.implicitConversions
  
  /** Implicitly promote exceptions to failures */
  implicit def promote(exception: Throwable) = apply(exception)
  
}

