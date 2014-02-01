package com.wbillingsley.handy

import scala.language.higherKinds

/**
 * A reference to an item by its id.
 * 
 * clazz is kept because it will be needed at run-time by the lookUp method (to know which class to look 
 * up in the database)
 */
case class RefById[T, K](val id: K, val lookUpMethod:LookUpOne[T, K]) extends Ref[T] with UnresolvedRef[T] with IdImmediate[T] {
    
  def immediateId[TT >: T, KK](implicit g:GetsId[TT, KK]) = {
    g.canonical(id)
  }

  def refId[TT >: T, KK](implicit g:GetsId[TT, KK]) = immediateId(g)

  def getId[TT >: T, KK](implicit g:GetsId[TT, KK]) = immediateId(g)

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

    def whichIs[T](r:Ref[T]) = new RefById(id, LookUpOne.AlwaysReturns(r))
  }

  def apply[K](id:K) = new JustId(id)

  def empty[T] = new RefById[T,Any](None, LookUp.empty)
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

object LookUpOne {

  /**
   * A look up that always returns the item you just gave it
   */
  case class AlwaysReturns[T](r:Ref[T]) extends LookUpOne[T, Any] {
    def lookUpOne[KK <: Any](r:RefById[T, KK]):Ref[T] = r
  }

}