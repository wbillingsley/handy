package com.wbillingsley.handy

import com.wbillingsley.handy._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.language.higherKinds

class RefFuture[+T](val future: Future[T]) extends Ref[T] {
  
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
  
  def onComplete[U](onSuccess: (T) => U, onNone: => U, onFail: Throwable => U) {
    future.onSuccess { 
      case v => onSuccess(v)
    }    
    future.onFailure {
      case n:NoSuchElementException => onNone
      case f => onFail(f) 
    }
  }   
  
  override def orIfNone[B >: T](f: => Ref[B]) = {
     val fut = future map { RefItself(_) } recover { case _ => f }
     new RefFutureRef(fut)
  }
    
  def flatMapOne[B](f: T => Ref[B]) = {	
    val result:Future[Ref[B]] = future.map(f)
    new RefFutureRef(result)    
  }
  
  def flatMapMany[B](f: T => RefMany[B]) = {	
    val result:Future[RefMany[B]] = future.map(f)
    new RefFutureRefMany(result)    
  }

  def map[B](f: (T) => B):Ref[B] = new RefFuture(future.map(f))
  
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
  
  def withFilter(p: T => Boolean) = new RefFuture(future.filter(p))
  
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
    new RefFutureRef({
    	val a = futureRef.map{ _ orIfNone f }     	
    	val b = a recover { case _ => f }
    	b
    })    
  }
  
  def map[B](f: (T) => B) = new RefFutureRef(futureRef.map(_.map(f)))
  
  def flatMapOne[B](f: T => Ref[B]) = {  
    new RefFutureRef(futureRef.map(_.flatMap(f)))
  }  
    
  def flatMapMany[B](f: T => RefMany[B]) = {  
    new RefFutureRefMany(futureRef.map(_.flatMap(f)))
  }  
  
  def withFilter(p: T => Boolean) = new RefFutureRef(futureRef.map(_.withFilter(p)))
 
  def onComplete[U](onSuccess: T => U, onNone: => U, onFail: Throwable => U) {
    futureRef onSuccess { case r:Ref[T] => r onComplete(onSuccess, onNone, onFail) }
    futureRef onFailure {
      case n:NoSuchElementException => onNone
      case f => onFail(f) 
    }
  }   
  
}


class RefFutureRefMany[+T](val futureRef: Future[RefMany[T]]) extends RefMany[T] {
  
  def isEmpty = fetch.isEmpty
  
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
  
  def map[B](f: (T) => B) = new RefFutureRefMany(futureRef.map(_.map(f)))
  
  def flatMapOne[B](f: T => Ref[B]) = {  
    new RefFutureRefMany(futureRef.map(_.flatMap(f)))
  }  
    
  def flatMapMany[B](f: T => RefMany[B]) = {  
    new RefFutureRefMany(futureRef.map(_.flatMap(f)))
  }  

  def withFilter(p: T => Boolean) = new RefFutureRefMany(futureRef.map(_.withFilter(p)))

  def fold[B](initial: =>B)(each:(B, T) => B) = new RefFutureRef(futureRef.map(_.fold(initial)(each)))
  
  override def onReady[U](onSuccess: RefMany[T] => U, onNone: => U, onFail: Throwable => U) {
    futureRef onSuccess { case r:RefMany[T] => r onReady(onSuccess, onNone, onFail) }
    futureRef onFailure {
      case n:NoSuchElementException => onNone
      case f => onFail(f) 
    }
  
  }
  
  def first = new RefFutureRef(futureRef.map(_.first))

  override def toRefOne = new RefFutureRef(futureRef.map(_.toRefOne))
}


class RefFutureOption[+T](val future: Future[Option[T]]) extends Ref[T] {
  
  def getId[TT >: T, KK](implicit g:GetsId[TT, KK]) = fetch.getId(g)
  
  def fetch = {
    try {
    	val t = Await.result(future, Duration.Inf)
    	Ref(t)
    } catch {
      case ex:Throwable => RefFailed(ex)
    }
  }
  
  def foreach[U](f: (T) => U) {
    future.foreach(_.foreach(f))
  }
  
  def onComplete[U](onSuccess: (T) => U, onNone: => U, onFail: Throwable => U) {
    future.onSuccess { 
      case r => r match {
        case Some(v) => onSuccess(v)
        case None => onNone
      }
    }    
    future.onFailure {
      case n:NoSuchElementException => onNone
      case f => onFail(f) 
    }
  }   
  
  override def orIfNone[B >: T](f: => Ref[B]) = {
     val fut = future map { Ref(_) } recover { case _ => f }
     new RefFutureRef(fut)
  }
    
  def flatMapOne[B](f: T => Ref[B]) = {	
    val result:Future[Ref[B]] = future.map(_ match {
      case Some(v) => f(v)
      case None => RefNone
    })
    new RefFutureRef(result)    
  }
  
  def flatMapMany[B](f: T => RefMany[B]) = {	
    val result:Future[RefMany[B]] = future.map(_ match {
      case Some(v) => f(v)
      case None => RefNone
    })
    new RefFutureRefMany(result)    
  }

  def map[B](f: (T) => B):Ref[B] = {
    new RefFutureOption(future.map(_.map(f)))
  }
  
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
  
  def withFilter(p: T => Boolean) = new RefFutureOption(future.map(_.filter(p)))
  
}
