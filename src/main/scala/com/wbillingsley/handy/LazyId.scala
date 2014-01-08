package com.wbillingsley.handy

/**
 * A Ref by id, that uses a lazy val to cache the result of looking it up
 */
class LazyId [T, K](clazz : scala.Predef.Class[T], id: K)(implicit val lookUpMethod:LookUpOne[T, K]) extends Ref[T] {

  val rbi = RefById(clazz, id)(lookUpMethod)
  
  lazy val lookUp = rbi.lookUp
  
  def getId[TT >: T, KK](implicit g:GetsId[TT, KK]) = {
    rbi.getId(g)
  }  
  
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
  
  def toOption = lookUp.toOption
  
  def orIfNone[B >: T](f: => Ref[B]):Ref[B] = lookUp.orIfNone(f)
  
  def recoverWith[B >: T](pf: PartialFunction[Throwable, Ref[B]]) = lookUp.recoverWith(pf)
  
  def withFilter(p: T => Boolean) = lookUp.withFilter(p)  
}
