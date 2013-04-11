package com.wbillingsley.handyplay

import com.wbillingsley.handy._
import play.api.libs.iteratee._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future

/**
 * A Ref that takes an Enumerator
 */
class RefEnumIter[T](val enumerator:Enumerator[Iterator[T]]) extends RefMany[T] {
  
  private def enumerateIterator(iterator:Iterator[T]) = {        
    Enumerator.generateM({
      if (iterator.hasNext) {
        Future.successful(Some(iterator.next))
      } else {
        Future.successful(None)
      }      
    })    
  }
  
  /** 
   * If you're using a non-blocking framework, you probably don't want to call this one!
   */
  def fetch = {
    
    import scala.collection.mutable
    
    val iteratee = Iteratee.fold[Iterator[T], mutable.Buffer[T]](mutable.Buffer.empty[T])((buf, t) => buf ++= t)    
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
    val iteratee = Iteratee.foreach[Iterator[T]](i => i.foreach(f))
    enumerator.run(iteratee)
  }
    
  def flatMapOne[B](f: T => Ref[B]) = {
    import RefConversions._
    
    val enum2 = enumerator &> Enumeratee.mapFlatten { iterator => 
      val inner = enumerateIterator(iterator)
      inner &> Enumeratee.mapFlatten(el => f(el).enumerate)
    }
    new RefEnumerator(enum2)
  }

  def flatMapMany[B](f: T => RefMany[B]) = {
    import RefConversions._
    
    val enum2 = enumerator &> Enumeratee.mapFlatten { iterator => 
      val inner = enumerateIterator(iterator)
      inner &> Enumeratee.mapFlatten(el => f(el).enumerate)
    }
    new RefEnumerator(enum2)
  }  
  
  def map[B](f: (T) => B) = new RefEnumIter(enumerator.map(i => i.map(f)))
  
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
    new RefEnumIter(enumerator &> Enumeratee.map(i => i.filter(p)))
  }

  def first: com.wbillingsley.handy.Ref[T] = {
    import RefConversions._
    
    val enum2 = enumerator &> Enumeratee.mapFlatten { iterator => 
      enumerateIterator(iterator)
    }
    new RefFutureOption(enum2 |>>> Iteratee.head)
    
  }
  
  def fold[B](initial: => B)(each: (B, T) => B): com.wbillingsley.handy.Ref[B] = {
    val enum2 = enumerator &> Enumeratee.mapFlatten { iterator => 
      enumerateIterator(iterator)
    }
    val iter = Iteratee.fold(initial)(each)
    new RefFuture(enum2 |>>> iter)    
  }
  
  def onReady[U](onSuccess: com.wbillingsley.handy.RefMany[T] => U,onNone: => U,onFail: Throwable => U): Unit = onSuccess(this)
  
}

object RefEnumIter {
  
  val timeout = 250.millis
  
}