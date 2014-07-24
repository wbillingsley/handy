package com.wbillingsley.handy

/**
 * A Ref by id, that uses a lazy val to cache the result of looking it up.
 * Generally you should use this
 */
case class LazyId [T, K](nakedId: K, lookUpMethod:LookUpOne[T, K]) extends Ref[T] with IdImmediate[T] {

  import Id._

  def id = nakedId.asId[T]
  
  lazy val lookUp = lookUpMethod(id)
  
  override def refId[K](implicit g:GetsId[T, K]) = immediateId(g)

  def getId[K](implicit g:GetsId[T, K]) = g.canonical[T](nakedId)

  override def immediateId[K](implicit g:GetsId[T, K]) = g.canonical[T](nakedId)
  
  def fetch = lookUp.fetch
  
  def foreach[U](f: (T) => U) {
    lookUp.foreach(f)
  }
  
  def onComplete[U](onSuccess: T => U, onNone: => U, onFail: Throwable => U) { 
    lookUp.onComplete(onSuccess, onNone, onFail) 
  }

  def toFuture = lookUp.toFuture

  def toFutOpt = lookUp.toFutOpt
  
  def flatMapOne[B](f: T => Ref[B]) = lookUp.flatMap(f)

  def flatMapMany[B](f: T => RefMany[B]) = lookUp.flatMap(f)  
  
  def map[B](f: (T) => B) = lookUp.map(f)
  
  def orIfNone[B >: T](f: => Ref[B]):Ref[B] = lookUp.orIfNone(f)
  
  def recoverWith[B >: T](pf: PartialFunction[Throwable, Ref[B]]) = lookUp.recoverWith(pf)
  
  def withFilter(p: T => Boolean) = lookUp.withFilter(p)  
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
