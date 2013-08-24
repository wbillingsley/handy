package com.wbillingsley.handy

/**
 * A reference that is expected to contain several items
 */
trait RefMany[+T] extends RSuper[T] {
  
  def fetch:ResolvedRefMany[T]
  
  def map[B](func: T => B):RefMany[B]  
  
  def foreach[U](func: T => U):Unit 

  def isEmpty: Boolean

  def flatMap[B, R[B], Result[B]](func: T => R[B])(implicit imp: RMCFMT[RefMany, R, Result]):Result[B] = imp.flatMap(this, func)

  def flatMapOne[B](func: T => Ref[B]):RefMany[B] 

  def flatMapMany[B](func: T => RefMany[B]):RefMany[B] 
  
  def withFilter(func: T => Boolean):RefMany[T] 
  
  def first:Ref[T]
  
  /**
   * A fold across this (possibly asynchronous) collection
   * initial will only be evaluated in the success case.
   */
  def fold[B](initial: =>B)(each:(B, T) => B):Ref[B]

  /**
   * Whereas with a RefOne, onComplete means any Future has completed, RefMany.onReady might only mean that the first found
   * entry is available (or that there is an Enumerator ready to stream results from the database).
   */
  def onReady[U](onSuccess: RefMany[T] => U, onNone: => U, onFail: Throwable => U):Unit  
  
  /**
   * Converts this to a reference to a collection
   */
  def toRefOne:Ref[TraversableOnce[T]] = {
    import scala.collection.mutable.Buffer    
    fold(Buffer.empty[T]){(buf, item) => buf.append(item); buf}
  }
  
}

object RefMany {
  
  /** 
   * FlatMap from many to many returns a RefMany.
   */
  implicit object ManyToMany extends RMCFMT[RefMany, RefMany, RefMany] {    
    def flatMap[A, B](from: RefMany[A], f: A => RefMany[B]) = {
      from.flatMapMany(f)
    }    
  }  
  
  
  /** 
   * FlatMap from many to one returns a RefMany.
   */
  implicit object ManyToOne extends RMCFMT[RefMany, Ref, RefMany] {    
    def flatMap[A, B](from: RefMany[A], f: A => Ref[B]) = {
      from.flatMapOne(f)
    }    
  }  
}