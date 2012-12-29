package com.wbillingsley.handy

import scala.language.higherKinds

/**
 * A reference to an item by its id.
 * 
 * clazz is kept because it will be needed at run-time by the lookUp method (to know which class to look 
 * up in the database)
 */
case class RefById[T, K](clazz : scala.Predef.Class[T], id: K) extends UnresolvedRef[T] with RefOne[T] {
    
  override def getId[TT >: T, KK](implicit g:GetsId[TT, KK]) = {
	g.canonical(id)
  }  

  def lookUp = RefById.lookUpMethod.lookup(this)
  
  def fetch = lookUp.fetch
  
  def foreach[U](f: (T) => U) {
    fetch.foreach(f)
  }
  
  def flatMap[B, R[B] >: RefNothing <: Ref[B]](f: T => R[B]) = fetch.flatMap(f)
  
  def map[B](f: (T) => B) = fetch.map(f)
  
  def toOption = fetch.toOption
  
  def getId = Some(id)
  
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
  
}


object RefById {

	trait LookUp {
	  def lookup[T](r:RefById[T,_]): RefOne[T]
	}
  
	/**
	 * If there's n
	 */
	val defaultLookUp = new LookUp {
	  def lookup[T](r:RefById[T,_]) = throw new UnsupportedOperationException("RefById.lookUpMethod has not been set")
	}
	
	/**
	 * Set this method to define how RefByIds are looked up. 
	 */
	var lookUpMethod:LookUp = defaultLookUp
}