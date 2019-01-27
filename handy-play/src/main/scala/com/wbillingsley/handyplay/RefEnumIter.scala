package com.wbillingsley.handyplay

import com.wbillingsley.handy._
import play.api.libs.iteratee._
import scala.concurrent.{ExecutionContext, Await, Future}
import scala.concurrent.duration._

/**
 * A Ref that takes an Enumerator
 */
class RefEnumIter[T](val enumerator:Enumerator[Iterator[T]])(implicit val executionContext:ExecutionContext) extends RefMany[T] {
  
  import RefFuture.executionContext
  
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
   *
   @Deprecated
  def fetch = {
    
    import scala.collection.mutable
    
    val iteratee = Iteratee.fold[Iterator[T], mutable.Buffer[T]](mutable.Buffer.empty[T])((buf, t) => buf ++= t)    
    val fr = enumerator.run(iteratee)
    try {
      val r = Await.result(fr, RefEnumerator.timeout)
      new RefTraversableOnce(r)
    } catch {
      case x:Throwable => 
        RefManyFailed(x)
    }
    
  }*/
  
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

  override def flatMapOpt[B](f: T => RefOpt[B]) = {   
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
    
  def withFilter(p: T => Boolean) = {
    new RefEnumIter(enumerator &> Enumeratee.map(i => i.filter(p)))
  }

  override def first: com.wbillingsley.handy.RefOpt[T] = {
    import RefConversions._
    import Ref._
    
    val enum2 = enumerator &> Enumeratee.mapFlatten { iterator => 
      enumerateIterator(iterator)
    }
    
    (enum2 |>>> Iteratee.head).toRefOpt
    
  }
  
  def foldLeft[B](initial: => B)(each: (B, T) => B): com.wbillingsley.handy.Ref[B] = {
    val enum2 = enumerator &> Enumeratee.mapFlatten { iterator => 
      enumerateIterator(iterator)
    }
    val iter = Iteratee.fold(initial)(each)
    new RefFuture(enum2 |>>> iter)    
  }
  
  def whenReady[B](block: RefMany[T] => B):Ref[B] = RefItself(block(this))
  
  def recoverManyWith[B >: T](pf: PartialFunction[Throwable, RefMany[B]]) = this
  
}

object RefEnumIter {
  
  val timeout = 250.millis
  
}