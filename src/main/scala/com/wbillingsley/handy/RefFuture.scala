package com.wbillingsley.handy

import com.wbillingsley.handy._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.language.higherKinds

class RefFuture[+T](val future: Future[T]) extends Ref[T] with RefOne[T] {
  
  def getId[TT >: T, KK](implicit g:GetsId[TT, KK]) = fetch.getId(g)
  
  def fetch = {
    try {
    	val t = Await.result(future, Duration.Inf)
    	RefItself(t)
    } catch {
      case ex:Throwable => RefFailed(ex)
    }
  }
  
  def foreach[U](f: (T) => U) {
    future.foreach(f)
  }
  
  override def orIfNone[B >: T](f: => Ref[B]) = {
     val fut = future map { RefItself(_) } recover { case _ => f }
     new RefFutureRef(fut)
  }
    
  def flatMap[B, R[B] >: RefNothing <: Ref[B]](f: T => R[B]) = {	
    val result:Future[R[B]] = future.map(f)
    new RefFutureRef(result)    
  }
  
  def map[B](f: (T) => B) = new RefFuture(future.map(f))
  
  def toOption = fetch.toOption
  
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
  
}


class RefFutureRef[+T](val futureRef: Future[Ref[T]]) extends Ref[T] {
  
  def getId[TT >: T, KK](implicit g:GetsId[TT, KK]) = fetch.getId(g)
  
  def isEmpty = fetch.isEmpty
  
  def toOption = fetch.toOption
  
  def fetch = {
    try {
    	val t = Await.result(futureRef, Duration.Inf)
    	t.fetch
    } catch {
      case ex:Throwable => RefFailed(ex)
    }
  }    
  
  def foreach[U](f: (T) => U) {
    futureRef.foreach(_.foreach(f))
  }  
  
  override def orIfNone[B >: T](f: => Ref[B]) = {
    new RefFutureRef(
    	futureRef.map{ _ orIfNone f } recover { case _ => f }
    )    
  }
  
  def map[B](f: (T) => B) = new RefFutureRef(futureRef.map(_.map(f)))
  
  def flatMap[B, R[B] >: RefNothing <: Ref[B]](f: T => R[B]) = {  
    new RefFutureRef(futureRef.map(_.flatMap(f)))
  }  
  
  
}
