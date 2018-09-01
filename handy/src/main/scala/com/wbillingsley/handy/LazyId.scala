package com.wbillingsley.handy

import scala.concurrent.Future

/**
 * A Ref by id, that uses a lazy val to cache the result of looking it up.
 * Generally you should use this
 */
case class LazyId [T, K](nakedId: K, lookUpMethod:LookUpOne[T, K]) extends Ref[T] with IdImmediate[T] {

  import Id._

  def id:Id[T,K] = nakedId.asId[T]
  
  lazy val lookUp = lookUpMethod(id)
  
  def getId[K](implicit g:GetsId[T, K]) = g.canonical[T](nakedId)

  override def immediateId[K](implicit g:GetsId[T, K]):Option[Id[T,K]] = g.canonical[T](nakedId)
  
  def foreach[U](f: (T) => U) {
    lookUp.foreach(f)
  }
  
  def toFuture:Future[T] = lookUp.toFuture

  def flatMapOne[B](f: T => Ref[B]):Ref[B] = lookUp.flatMapOne(f)

  override def flatMapOpt[B](func: T => RefOpt[B]): RefOpt[B] = lookUp.flatMapOpt(func)

  def flatMapMany[B](f: T => RefMany[B]):RefMany[B] = lookUp.flatMapMany(f)
  
  def map[B](f: (T) => B):Ref[B] = lookUp.map(f)
  
  def recoverWith[B >: T](pf: PartialFunction[Throwable, Ref[B]]):Ref[B] = lookUp.recoverWith(pf)

}

object LazyId {

  class JustType[T] {
    def apply[K](id: K)(implicit lookUpMethod:LookUp[T, K]) = new LazyId[T, K](id, lookUpMethod.one)
  }

  def of[T] = new JustType[T]

  class JustId[K](val id:K) extends AnyVal {
    def apply[T](implicit lookUp:LookUp[T, K]) = new LazyId[T,K](id, lookUp.one)
    def of[T](implicit lookUp:LookUp[T, K]) = apply(lookUp)
    def ofOne[T](implicit lookUp:LookUpOne[T, K]) = new LazyId(id, lookUp)
  }

  def apply[K](id:K) = new JustId(id)

  def empty[T] = new LazyId[T,Any](None, LookUp.empty[T,Any].one)

}
