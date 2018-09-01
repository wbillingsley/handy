package com.wbillingsley.handy

import scala.concurrent.Future
import scala.language.higherKinds
import scala.concurrent.ExecutionContext

object RefFuture {
  /**
   * The execution context that new RefFuture
   */
  var executionContext:ExecutionContext = ExecutionContext.Implicits.global
}

case class RefFuture[+T](future: Future[T])(implicit val executionContext:ExecutionContext) extends Ref[T] {
  
  /**
   * To avoid programs being non-deterministic, we return None even if the Future has already completed
   */
  override def immediateId[K](implicit g:GetsId[T, K]):None.type = None

  def foreach[U](f: (T) => U) {
    future.foreach(f)
  }

  override def toFuture:Future[T] = future

  override def recoverWith[B >: T](pf: PartialFunction[Throwable, Ref[B]]):Ref[B] = {
     val fut = future.map(RefItself.apply).recover(pf)
     RefFutureRef(fut)
  }
    
  def flatMapOne[B](f: T => Ref[B]):Ref[B] = {
    val result:Future[Ref[B]] = future.map(f)
    RefFutureRef(result)
  }

  override def flatMapOpt[B](func: T => RefOpt[B]): RefOpt[B] = {
    RefFutureRefOpt(future.map(func))
  }

  def flatMapMany[B](f: T => RefMany[B]):RefMany[B] = {
    val result:Future[RefMany[B]] = future.map(f)
    RefFutureRefMany(result)
  }


  def map[B](f: (T) => B):Ref[B] = new RefFuture(future.map(f))

}


case class RefFutureRef[+T](futureRef: Future[Ref[T]])(implicit val executionContext:ExecutionContext = RefFuture.executionContext) extends Ref[T] {

  /**
   * To avoid programs being non-deterministic, we return None even if the Future has already completed
   */
  override def immediateId[K](implicit g:GetsId[T, K]):None.type = None

  override def foreach[U](f: (T) => U) {
    futureRef.foreach(_.foreach(f))
  }  

  override def recoverWith[B >: T](pf: PartialFunction[Throwable, Ref[B]]):Ref[B] = {
    RefFutureRef({
      val a = futureRef.map{ _ recoverWith pf }     	
      val b = a recover pf
      b
    })    
  }
  
  override def map[B](f: (T) => B) = RefFutureRef(futureRef.map(_.map(f)))
  
  override def flatMapOne[B](f: T => Ref[B]):Ref[B] = {
    RefFutureRef(futureRef.map(_.flatMapOne(f)))
  }

  override def flatMapOpt[B](func: T => RefOpt[B]): RefOpt[B] = {
    RefFutureRefOpt(futureRef.map(_.flatMapOpt(func)))
  }

  override def flatMapMany[B](f: T => RefMany[B]):RefMany[B] = {
    RefFutureRefMany(futureRef.map(_.flatMapMany(f)))
  }  
  
  override def toFuture:Future[T] = futureRef.flatMap(_.toFuture)

}


case class RefFutureRefMany[+T](futureRef: Future[RefMany[T]])(implicit val executionContext:ExecutionContext = RefFuture.executionContext) extends RefMany[T] {
  
  override def foreach[U](f: T => U) {
    futureRef.foreach(_.foreach(f))
  }  
  
  override def map[B](f: T => B):RefMany[B] = {
    RefFutureRefMany(futureRef.map(_.map(f)))
  }
  
  override def flatMapOne[B](f: T => Ref[B]):RefMany[B] = {
    RefFutureRefMany(futureRef.map(_.flatMapOne(f)))
  }

  override def flatMapOpt[B](f: T => RefOpt[B]):RefMany[B] = {
    RefFutureRefMany(futureRef.map(_.flatMapOpt(f)))
  }

  override def flatMapMany[B](f: T => RefMany[B]):RefMany[B] = {
    RefFutureRefMany(futureRef.map(_.flatMapMany(f)))
  }  

  def withFilter(p: T => Boolean):RefMany[T] = RefFutureRefMany(futureRef.map(_.withFilter(p)))

  override def foldLeft[B](initial: =>B)(each:(B, T) => B):Ref[B] = {
    RefFutureRef(futureRef.map(_.foldLeft(initial)(each)))
  }

  def whenReady[B](block: RefMany[T] => B):Ref[B] = RefFutureRef(futureRef.map(_.whenReady(block)))
  
  override def recoverManyWith[B >: T](pf: PartialFunction[Throwable, RefMany[B]]):RefMany[B] = {
    val a = futureRef.map{ _ recoverManyWith pf }
    val b = a recover pf
    new RefFutureRefMany(b)
  }

  def first:RefOpt[T] = RefFutureRefOpt(futureRef.map(_.first))

  def takeWhile(p: T => Boolean):RefMany[T] = RefFutureRefMany(futureRef.map(_.takeWhile(p)))

}


case class RefFutureRefOpt[+T](future: Future[RefOpt[T]])(implicit val executionContext:ExecutionContext = RefFuture.executionContext) extends RefOpt[T] {

  override def require:Ref[T] = RefFutureRef(future.map(_.require))

  override def toFutureOpt:Future[Option[T]] = future.flatMap(ro => ro.toFutureOpt)

  override def orElse[B >: T](f: => RefOpt[B]):RefOpt[B] = {
     val fut = future.map(_.orElse(f))
     RefFutureRefOpt(fut)
  }
  
  override def recoverWith[B >: T](pf: PartialFunction[Throwable, RefOpt[B]]):RefOpt[B] = {
     RefFutureRefOpt(future.recover(pf))
  }
    
  def flatMapOne[B](f: T => Ref[B]):RefOpt[B] = {
    RefFutureRefOpt(future.map(_.flatMapOne(f)))
  }

  override def flatMapOpt[B](func: T => RefOpt[B]): RefOpt[B] = {
    RefFutureRefOpt(future.map(_.flatMapOpt(func)))
  }

  def flatMapMany[B](f: T => RefMany[B]):RefMany[B] = {
    RefFutureRefMany(future.map(_.flatMapMany(f)))
  }

  override def map[B](f: (T) => B):RefOpt[B] = {
    RefFutureRefOpt(future.map(_.map(f)))
  }

  def withFilter(p: T => Boolean):RefOpt[T] = {
    RefFutureRefOpt(future.map(_.withFilter(p)))
  }

}
