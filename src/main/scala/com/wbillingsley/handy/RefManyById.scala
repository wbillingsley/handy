package com.wbillingsley.handy

import Ref._

/**
 * Refers to many items by their ID.
 */
class RefManyById[T, K](val clazz : scala.Predef.Class[T], val rawIds: Seq[K]) extends RefMany[T] {
  
  def getIds[TT >: T, KK](implicit g:GetsId[TT, KK]) = {
	for (i <- rawIds; c <- g.canonical(i)) yield c
  }  
  
  def lookUp = RefManyById.lookUpMethod.lookup(this)  
  
  def first = Ref.fromOptionId(clazz, rawIds.headOption)
  
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

  trait LookUp {
    def lookup[T](r: RefManyById[T, _]): RefMany[T]
  }

  /**
   * The default lookup is not set. It would be possible to by default fetch each item in turn, but that
   * is not preferred. 
   */
  val defaultLookUp = new LookUp {
    def lookup[T](r: RefManyById[T, _]) = RefFailed(new UnsupportedOperationException("RefManyById.lookUpMethod not set"))
  }

  /**
   * Set this method to define how RefByIds are looked up.
   */
  var lookUpMethod: LookUp = defaultLookUp

}
