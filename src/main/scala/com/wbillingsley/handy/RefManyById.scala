package com.wbillingsley.handy

import Ref._

/**
 * Refers to many items by their ID.
 */
case class RefManyById[T, K](val clazz : scala.Predef.Class[T], val rawIds: Seq[K])(implicit lookUpMany:LookUpMany[T, K], val lookUpOne:LookUpOne[T, K]) extends RefMany[T] {
  
  def getIds[TT >: T, KK](implicit g:GetsId[TT, KK]) = {
	for (i <- rawIds; c <- g.canonical(i)) yield c
  }  
  
  def lookUp = lookUpMany.lookUpMany(this)
  
  def first = Ref.fromOptionId(clazz, rawIds.headOption)(lookUpOne)
  
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
  
  def LooksUpNothing[T, K] = new LookUp[T, K] {
    override def lookUpOne(r:RefById[T,K]) = RefNone
    
    override def lookUpMany(r:RefManyById[T,K]) = RefNone
  }
  
  def empty[T, K](clazz: scala.Predef.Class[T]) = {
    val nada = LooksUpNothing[T, K]
    new RefManyById(clazz, Seq.empty)(nada, nada)
  }
  
}


/**
 * This has to be a trait, rather than just a function because a RefManyById[T, K] is already a RefMany[T].
 * Which means that if it was just a function, then Predef.conforms would be implicitly found.
 */
trait LookUpMany[T, K] {
  def lookUpMany(r:RefManyById[T, K]):RefMany[T]
}

trait LookUp[T, K] extends LookUpMany[T, K] with LookUpOne[T, K]
