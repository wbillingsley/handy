package com.wbillingsley.handy

import Ref._

/**
 * Refers to many items by their ID.
 */
case class RefManyById[T, K](val rawIds: Seq[K], lu:LookUp[T, K]) extends RefMany[T] {

  import Ids._

  def getIds = rawIds.asIds[T]

  def lookUp = lu.many(getIds)
  
  def first = Ref.fromOptionId(rawIds.headOption)(lu)
  
  def fetch = lookUp.fetch
    
  def isEmpty = rawIds.isEmpty
  
  def map[B](f: T => B) = lookUp.map(f)
  
  def flatMapMany[B](f: T => RefMany[B]) = lookUp.flatMapMany(f)

  def flatMapOne[B](f: T => Ref[B]) = lookUp.flatMapOne(f)
  
  def foreach[U](f: T => U) { lookUp.foreach(f) }
  
  def withFilter(p: T => Boolean) = lookUp withFilter p
  
  def fold[B](initial: =>B)(each:(B, T) => B) = lookUp.fold(initial)(each)
 
  def whenReady[B](block: RefMany[T] => B):Ref[B] = lookUp.whenReady(block)
  
  def recoverManyWith[B >: T](pf: PartialFunction[Throwable, RefMany[B]]) = lookUp.recoverManyWith(pf)

}

object RefManyById {

  /**
   * An empty RefMany that always resolves to nothing.
   *
   * Note that it takes two type parameters as RefManyById is invariant in K (so we can't just
   * return a RefManyById[T, Any]
   */
  def empty[T,K] = {
    val nada = LookUp.empty[T,K]
    new RefManyById[T,K](Seq.empty,nada)
  }

  class JustType[T] {
    def apply[K](ids: Seq[K])(implicit lookUpMethod:LookUp[T, K]) = RefManyById(ids, lookUpMethod)
  }

  def of[T] = new JustType[T]

  class JustId[K](val ids: Seq[K]) extends AnyVal {
    def apply[T](implicit lookUpMethod:LookUp[T, K]) = RefManyById(ids, lookUpMethod)
    def of[T](implicit lookUpMethod:LookUp[T, K]) = apply(lookUpMethod)
  }

  def apply[K](ids: Seq[K]) = new JustId(ids)
  
}
