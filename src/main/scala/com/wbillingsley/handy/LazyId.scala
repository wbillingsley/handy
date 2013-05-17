package com.wbillingsley.handy

/**
 * A Ref by id, that uses a lazy val to cache the result of looking it up
 */
class LazyId [T, K](clazz : scala.Predef.Class[T], id: K) extends Ref[T] {

  val rbi = RefById(clazz, id)
  
  lazy val lookUp = rbi.lookUp
  
  override def getId[TT >: T, KK](implicit g:GetsId[TT, KK]) = {
	g.canonical(id)
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
  
  def getId = Some(id)
  
  def isEmpty = lookUp.isEmpty	
  
  def withFilter(p: T => Boolean) = lookUp.withFilter(p)  
}
