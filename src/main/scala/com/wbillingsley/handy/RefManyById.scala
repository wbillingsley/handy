package com.wbillingsley.handy

import Ref._

/**
 * Refers to many items by their ID.
 */
case class RefManyById[T, K](val rawIds: Seq[K], lookUpMany:LookUpMany[T, K], lookUpOne:LookUpOne[T, K]) extends RefMany[T] {
  
  def getIds[TT >: T, KK](implicit g:GetsId[TT, KK]) = {
	for (i <- rawIds; c <- g.canonical(i)) yield c
  }  
  
  def lookUp = lookUpMany.lookUpMany(this)
  
  def first = Ref.fromOptionId(rawIds.headOption)(lookUpOne)
  
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

  def apply[T,K](rawIds:Seq[K], lookUp:LookUp[T, K]):RefManyById[T,K] = apply(rawIds, lookUp, lookUp)
  
  def LooksUpNothing[T] = new LookUp[T, Any] {
    override def lookUpOne[KK <: Any](r:RefById[T,KK]) = RefNone
    
    override def lookUpMany[KK <: Any](r:RefManyById[T,KK]) = RefNone
  }

  /**
   * An empty RefMany that always resolves to nothing.
   *
   * Note that it takes two type parameters as RefManyById is invariant in K (so we can't just
   * return a RefManyById[T, Any]
   */
  def empty[T,K] = {
    val nada = LooksUpNothing[T]
    new RefManyById[T,K](Seq.empty,nada, nada)
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


/**
 * This has to be a trait, rather than just a function because a RefManyById[T, K] is already a RefMany[T].
 * Which means that if it was just a function, then Predef.conforms would be implicitly found.
 */
trait LookUpMany[T, -K] {
  def lookUpMany[KK <: K](r:RefManyById[T, KK]):RefMany[T]
}

trait LookUp[T, -K] extends LookUpMany[T, K] with LookUpOne[T, K]
