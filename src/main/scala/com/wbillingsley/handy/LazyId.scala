package com.wbillingsley.handy

/**
 * A Ref by id, that uses a lazy val to cache the result of looking it up.
 * Generally you should use this
 */
case class LazyId [T, K](id: K, lookUpMethod:LookUpOne[T, K]) extends Ref[T] with IdImmediate[T] {

  val rbi = new RefById(id, lookUpMethod)
  
  lazy val lookUp = rbi.lookUp
  
  def refId[TT >: T, KK](implicit g:GetsId[TT, KK]) = rbi.refId(g)

  def getId[TT >: T, KK](implicit g:GetsId[TT, KK]) = rbi.getId(g)

  def immediateId[TT >: T, KK](implicit g:GetsId[TT, KK]) = rbi.immediateId(g)
  
  def fetch = lookUp.fetch
  
  def foreach[U](f: (T) => U) {
    lookUp.foreach(f)
  }
  
  def onComplete[U](onSuccess: T => U, onNone: => U, onFail: Throwable => U) { 
    lookUp.onComplete(onSuccess, onNone, onFail) 
  }
  
  def flatMapOne[B](f: T => Ref[B]) = lookUp.flatMap(f)

  def flatMapMany[B](f: T => RefMany[B]) = lookUp.flatMap(f)  
  
  def map[B](f: (T) => B) = lookUp.map(f)
  
  def orIfNone[B >: T](f: => Ref[B]):Ref[B] = lookUp.orIfNone(f)
  
  def recoverWith[B >: T](pf: PartialFunction[Throwable, Ref[B]]) = lookUp.recoverWith(pf)
  
  def withFilter(p: T => Boolean) = lookUp.withFilter(p)  
}

object LazyId {

  class JustType[T] {
    def apply[K](id: K)(implicit lookUpMethod:LookUpOne[T, K]) = new LazyId[T, K](id, lookUpMethod)
  }

  def of[T] = new JustType[T]

  class JustId[K](val id:K) extends AnyVal {
    def apply[T](implicit lookUpMethod:LookUpOne[T, K]) = new LazyId(id, lookUpMethod)
    def of[T](implicit lookUpMethod:LookUpOne[T, K]) = apply(lookUpMethod)

    def whichIs[T](r:Ref[T]) = new LazyId(id, LookUpOne.AlwaysReturns(r))
  }

  def apply[K](id:K) = new JustId(id)

  def empty[T] = new LazyId[T,Any](None, LookUp.empty)

}
