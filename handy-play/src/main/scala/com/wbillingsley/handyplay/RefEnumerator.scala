package com.wbillingsley.handyplay

import com.wbillingsley.handy._
import play.api.libs.iteratee.{Iteratee, Enumerator, Enumeratee, Input}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future

/**
 * A Ref that takes an Enumerator
 */
class RefEnumerator[T](val enumerator:Enumerator[T]) extends RefMany[T] {
  
  import RefFuture.executionContext
  
  /** 
   * If you're using a non-blocking framework, you probably don't want to call this one!
   */
  def fetch = {
    import scala.collection.mutable
    
    val iteratee = Iteratee.fold[T, mutable.Buffer[T]](mutable.Buffer.empty[T])((buf, t) => buf += t)    
    val fr = enumerator.run(iteratee)
    try {
      val r = Await.result(fr, RefEnumerator.timeout)
      new RefTraversableOnce(r)
    } catch {
      case x:Throwable => 
        RefFailed(x)
    }
    
  }
  
  def foreach[U](f: (T) => U) {
    val iteratee = Iteratee.foreach[T](f(_))
    enumerator.run(iteratee)
  }
    
  def flatMapOne[B](f: T => Ref[B]) = {   
    import RefConversions._
    val enum2 = enumerator.flatMap(f(_).enumerate)
    new RefEnumerator(enum2)
  }
  
  def flatMapMany[B](f: T => RefMany[B]) = {   
    import RefConversions._
    val enum2 = enumerator.flatMap(f(_).enumerate)
    new RefEnumerator(enum2)
  }  
  
  def map[B](f: (T) => B) = new RefEnumerator(enumerator.map(f))
  
  def isTraversableAgain = false
  
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
  
  def withFilter(p: T => Boolean) = {
    new RefEnumerator(enumerator &> Enumeratee.filter(p))
  }

  def first: com.wbillingsley.handy.Ref[T] = {
    val iter = Iteratee.head[T]
    val r = enumerator |>>> iter
    new RefFutureOption(r)    
  }
  
  def fold[B](initial: => B)(each: (B, T) => B): com.wbillingsley.handy.Ref[B] = {
    val iter = Iteratee.fold(initial)(each)
    new RefFuture(enumerator |>>> iter)
  }
  
  def onReady[U](onSuccess: com.wbillingsley.handy.RefMany[T] => U,onNone: => U,onFail: Throwable => U): Unit = onSuccess(this)
  
}

/*
class RefEnumeratorRef[T](val enumerator:Enumerator[Ref[T]]) extends RefMany[T] {
  
  /** 
   * If you're using a non-blocking framework, you probably don't want to call this one!
   */
  def fetch = {
    
    val iteratee = Iteratee.fold[Ref[T], Seq[T]](Seq.empty[T])((seq, t) =>
      t.toOption match {
        case Some(el) => seq :+ el
        case None => seq
      }      
    )    
    val fr = enumerator.run(iteratee)
    try {
      val r = Await.result(fr, RefEnumerator.timeout)
      new RefTraversableOnce(r)
    } catch {
      case x:Throwable => 
        RefFailed(x)
    }
    
  }
  
  def foreach[U](f: (T) => U) {
    val iteratee = Iteratee.foreach[Ref[T]](el => el.foreach(f))
    enumerator.run(iteratee)
  }
    
  def flatMapOne[B](f: T => Ref[B]) = {   
    val enum2 = enumerator.map(r => for (item <- r; res <- f(item)) yield res)    
    new RefEnumeratorRef(enum2)
  }
  
  def flatMapMany[B](f: T => RefMany[B]) = {   
    val enum2 = enumerator.map(r => for (item <- r; res <- f(item)) yield res)  
    new RefEnumeratorRefMany(enum2)
  }  
  
  def map[B](f: (T) => B) = {
    val enum2 = enumerator.map(r => for (item <- r) yield f(item))
    new RefEnumeratorRef(enum2)
  }
  
  def isTraversableAgain = false
  
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
  
  def withFilter(p: T => Boolean) = {
    import RefConversions._
        
    val enum2 = enumerator &> Enumeratee.mapFlatten(_.enumerate)    
    new RefEnumerator(enum2 &> Enumeratee.filter(p))
  }

  def first: com.wbillingsley.handy.Ref[T] = {
    import RefConversions._
    
    val enum2 = enumerator &> Enumeratee.mapFlatten(_.enumerate)
    val iter = Iteratee.head[T]
    val r = enum2 |>>> iter
    new RefFutureOption(r)
  }
  
  def fold[B](initial: => B)(each: (B, T) => B): com.wbillingsley.handy.Ref[B] = {
    import RefConversions._
    
    val iter = Iteratee.fold(initial)(each)
    val enum2 = enumerator &> Enumeratee.mapFlatten(_.enumerate)
    new RefFuture(enum2 |>>> iter)
  }
  
  def onReady[U](onSuccess: com.wbillingsley.handy.RefMany[T] => U,onNone: => U,onFail: Throwable => U): Unit = ???
  
}

class RefEnumeratorRefMany[T](val enumerator:Enumerator[RefMany[T]]) extends RefMany[T] {
  
  /** 
   * If you're using a non-blocking framework, you probably don't want to call this one!
   */
  def fetch = {
    
    val iteratee = Iteratee.fold[RefMany[T], Seq[T]](Seq.empty[T])((seq, t) =>
      t.toOption match {
        case Some(el) => seq :+ el
        case None => seq
      }      
    )    
    val fr = enumerator.run(iteratee)
    try {
      val r = Await.result(fr, RefEnumerator.timeout)
      new RefTraversableOnce(r)
    } catch {
      case x:Throwable => 
        RefFailed(x)
    }
    
  }
  
  def foreach[U](f: (T) => U) {
    val iteratee = Iteratee.foreach[RefMany[T]](el => el.foreach(f))
    enumerator.run(iteratee)
  }
    
  def flatMapOne[B](f: T => Ref[B]) = {   
    val enum2 = enumerator.map(r => for (item <- r; res <- f(item)) yield res)    
    new RefEnumeratorRefMany(enum2)
  }
  
  def flatMapMany[B](f: T => RefMany[B]) = {   
    val enum2 = enumerator.map(r => for (item <- r; res <- f(item)) yield res)  
    new RefEnumeratorRefMany(enum2)
  }  
  
  def map[B](f: (T) => B) = {
    import RefConversions._
    
    val res = for (ref <- enumerator; el <- ref.enumerate) yield f(el)
    new RefEnumerator(res)
  }
  
  def isTraversableAgain = false
  
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
  
  def withFilter(p: T => Boolean) = {
    import RefConversions._
        
    val enum2 = enumerator &> Enumeratee.mapFlatten(_.enumerate)    
    new RefEnumerator(enum2 &> Enumeratee.filter(p))
  }

  def first: com.wbillingsley.handy.Ref[T] = {
    import RefConversions._
    
    val enum2 = enumerator &> Enumeratee.mapFlatten(_.enumerate)
    val iter = Iteratee.head[T]
    val r = enum2 |>>> iter
    new RefFutureOption(r)
  }
  
  def fold[B](initial: => B)(each: (B, T) => B): com.wbillingsley.handy.Ref[B] = {
    import RefConversions._
    
    val iter = Iteratee.fold(initial)(each)
    val enum2 = enumerator &> Enumeratee.mapFlatten(_.enumerate)
    new RefFuture(enum2 |>>> iter)
  }
  
  def onReady[U](onSuccess: com.wbillingsley.handy.RefMany[T] => U,onNone: => U,onFail: Throwable => U): Unit = ???
  
}
*/

object RefEnumerator {
  
  val timeout = 250.millis
  
}