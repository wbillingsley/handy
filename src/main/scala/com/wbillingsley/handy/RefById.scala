package com.wbillingsley.handy

import scala.language.higherKinds

/**
 * A reference to an item by its id.
 * 
 * clazz is kept because it will be needed at run-time by the lookUp method (to know which class to look 
 * up in the database)
 */
case class RefById[T, K](val id: K, val lookUpMethod:LookUpOne[T, K]) extends Ref[T] with UnresolvedRef[T] {
    
  def getId[TT >: T, KK](implicit g:GetsId[TT, KK]) = {
    g.canonical(id)
  }  

  def lookUp = lookUpMethod.lookUpOne(this)
  
  def fetch = lookUp.fetch
  
  def foreach[U](f: (T) => U) {
    lookUp.foreach(f)
  }
  
  def orIfNone[B >: T](f: => Ref[B]):Ref[B] = lookUp.orIfNone(f)
  
  def recoverWith[B >: T](pf: PartialFunction[Throwable, Ref[B]]) = lookUp.recoverWith(pf)
  
  def onComplete[U](onSuccess: T => U, onNone: => U, onFail: Throwable => U) { 
    lookUp.onComplete(onSuccess, onNone, onFail) 
  }
  
  def flatMapOne[B](f: T => Ref[B]) = lookUp.flatMap(f)

  def flatMapMany[B](f: T => RefMany[B]) = lookUp.flatMap(f)  
  
  def map[B](f: (T) => B) = fetch.map(f)
  
  def toOption = fetch.toOption
  
  def isTraversableAgain = true
  
  def toIterator = fetch.toIterator
  
  def toStream = fetch.toStream
  
  def copyToArray[B >: T](xs:Array[B], start:Int, len:Int) { 
    fetch.copyToArray(xs, start, len) 
  }
  
  def exists(p: T => Boolean) = fetch.exists(p)
  
  def find(p: T => Boolean) = fetch.find(p)
  
  def forall(p: T => Boolean) = fetch.forall(p)
  
  def hasDefiniteSize = fetch.hasDefiniteSize
  
  def seq = fetch.seq
  
  def toTraversable = fetch.toTraversable   
  
  def isEmpty = fetch.isEmpty	
  
  def withFilter(p: T => Boolean) = lookUp.withFilter(p)
  
}

object RefById {
  
  class JustType[T] {
    def apply[K](id: K)(implicit lookUpMethod:LookUpOne[T, K]) = new RefById[T, K](id, lookUpMethod)
  }
  
  def of[T] = new JustType[T]

  class JustId[K](val id:K) extends AnyVal {
    def apply[T](implicit lookUpMethod:LookUpOne[T, K]) = new RefById(id, lookUpMethod)

    def of[T](implicit lookUpMethod:LookUpOne[T, K]) = apply(lookUpMethod)
  }

  def apply[K](id:K) = new JustId(id)

}


/**
 * This has to be a trait, rather than just a function because a RefManyById[T, K] is already a RefMany[T].
 * Which means that if it was just a function, then Predef.conforms would be implicitly found.
 *
 * It also has to be invariant in T because two RefByIds are equal if they have the same Id and LookUpMethod
 *
 * It is covariant in K so that a LookUp[Foo, Any] can be used in place of a LookUp[Foo, String]
 */
trait LookUpOne[T, -K] {
  def lookUpOne[KK <: K](r:RefById[T, KK]):Ref[T]
}