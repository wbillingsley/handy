package com.wbillingsley.handy

import java.lang.Throwable

import com.wbillingsley.handy.reactivestreams._
import org.reactivestreams.{Processor, Subscription, Subscriber, Publisher}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}


/**
  *
  */
class RefPublisher[+T](publisher:Publisher[T])(implicit val ec: ExecutionContext) extends RefMany[T] {

  /**
    * Recovers from failures producing the list -- for instance if this is a RefFailed, or a RefFutureRefMany that fails.
    * Note that it does not recover from individual elements within the list failing.
    */
  override def recoverManyWith[B >: T](pf: PartialFunction[Throwable, RefMany[B]]): RefMany[B] = this

  override def withFilter(func: (T) => Boolean): RefMany[T] = {
    new RefPublisher(new MapR[T,T](publisher)({ item:T =>
      if (func(item)) RefSome(item) else RefNone
    })(ec))
  }

  /**
    * A fold across this (possibly asynchronous) collection
    * initial will only be evaluated in the success case.
    */
  override def foldLeft[B](initial: => B)(each: (B, T) => B): Ref[B] = new RefFuture[B](new FoldProcessor(publisher)(initial)(each).toFuture)

  override def map[B](func: (T) => B): RefMany[B] = flatMapOne { x => RefItself(func(x)) }

  override def flatMapOne[B](func: (T) => Ref[B]): RefMany[B] = {
    new RefPublisher(new MapR(publisher)(func.andThen(_.toRefOpt)))
  }

  override def flatMapOpt[B](func: T => RefOpt[B]): RefMany[B] = {
    new RefPublisher(new MapR(publisher)(func))
  }

  override def flatMapMany[B](func: (T) => RefMany[B]): RefMany[B] = {
    val publisherOfPubs:Publisher[Publisher[B]] = new MapR(publisher)({ x => RefSome(new RMPublisher(func(x))) })
    //val manyPubs = map(func.andThen(new RMPublisher(_)))
    val concatenated:Publisher[B] = new ConcatProcessor(publisherOfPubs)
    new RefPublisher(concatenated)
  }

  override def foreach[U](func: (T) => U): Unit = flatMapOne { x => RefItself(func(x)) }

  /**
    * Called when the RefMany is "ready". This is equivalent to
    * fold(initial){ (_,_) => initial } but without calling the empty folder for each value
    */
  override def whenReady[B](f: (RefMany[T]) => B): Ref[B] = RefItself(f(this))

  override def first: RefOpt[T] = ProcessorFuncs.headOption(publisher)

}



/**
  * A publisher that publishes a RefMany
  */
class RMPublisher[T](rm:RefMany[T])(implicit val ec: ExecutionContext) extends Publisher[T] {

  def start(s:DCSubscription[T]):Future[Boolean] = {
    rm.foldLeft(Future.successful(false))({ case (f, i) =>
      for {
        prevFinished <- f
        thisOne <- s.push(i)
      } yield false
    }).toFuture.flatMap(identity)
  }

  override def subscribe(s: Subscriber[_ >: T]): Unit = {
    val subscription = new DCSubscription[T](s)
    start(subscription).andThen({
      case Success(_) => s.onComplete()
      case Failure(x) => s.onError(x)
    })
    s.onSubscribe(subscription)
  }
}



