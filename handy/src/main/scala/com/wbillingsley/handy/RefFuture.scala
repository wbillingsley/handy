package com.wbillingsley.handy

import scala.concurrent.Future
import scala.language.higherKinds
import scala.concurrent.ExecutionContext
import scala.util.Success

object RefFuture {
  /**
   * The execution context that new RefFuture
   */
  var executionContext:ExecutionContext = ExecutionContext.Implicits.global
}

case class RefFuture[+T](future: Future[T])(implicit val executionContext:ExecutionContext) extends Ref[T] {

  override def immediateId[TT >: T, Key <: Id[TT, _]](implicit g:GetsId[TT, Key]):Option[Key] = {
    if (future.isCompleted) {
      future.value match {
        case Some(Success(v)) => g.getId(v)
        case _ => None
      }
    } else None
  }

  def foreach[U](f: (T) => U):Unit = {
    future.foreach(f)
  }

  override def toFuture:Future[T] = future

  override def recoverWith[B >: T](pf: PartialFunction[Throwable, Ref[B]]):Ref[B] = {
     val fut = future.map(RefItself.apply).recover(pf)
     RefFutureRef(fut)
  }
    
  def flatMapOne[B](f: T => Ref[B]):Ref[B] = {
    RefFuture(future.flatMap(f.andThen(_.toFuture)))
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

  override def immediateId[TT >: T, Key <: Id[TT, _]](implicit g:GetsId[TT, Key]):Option[Key] = {
    if (futureRef.isCompleted) {
      futureRef.value match {
        case Some(Success(v)) => v.immediateId(using g)
        case _ => None
      }
    } else None
  }

  override def foreach[U](f: (T) => U):Unit = {
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
  
  override def foreach[U](f: T => U):Unit = {
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

  override def iterator: RefOpt[RefIterator[T]] = RefFutureRefOpt(futureRef.map(_.iterator))

  //def takeWhile(p: T => Boolean):RefMany[T] = RefFutureRefMany(futureRef.map(_.takeWhile(p)))

}


case class RefFutureRefOpt[+T](future: Future[RefOpt[T]])(implicit val executionContext:ExecutionContext = RefFuture.executionContext) extends RefOpt[T] {

  override def require:Ref[T] = RefFutureRef(future.map(_.require))

  override def toFutureOpt:Future[Option[T]] = future.flatMap(ro => ro.toFutureOpt)

  override def orElse[B >: T](f: => RefOpt[B]):RefOpt[B] = {
     val fut = future.map(_.orElse(f))
     RefFutureRefOpt(fut)
  }
  
  override def recoverWith[B >: T](pf: PartialFunction[Throwable, RefOpt[B]]):RefOpt[B] = {
    val a = future.map { _ recoverWith pf }
    val b = a recover pf
    RefFutureRefOpt(b)
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

  override def option: Ref[Option[T]] = RefFuture(future.flatMap(_.toFutureOpt))
}
