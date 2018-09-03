package com.wbillingsley.handy.reactivestreams

import com.wbillingsley.handy._
import org.reactivestreams.{Processor, Publisher, Subscriber, Subscription}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

/**
  * A Processor that (possibly asynchronously) processes each item.
  * RefItself(item) => item will be passed on
  * RefNone => item will be skipped
  * RefFailed(x) =>
  */
object ProcessorFuncs {


  def headOption[T](publisher: Publisher[T])(implicit ec:ExecutionContext):RefOpt[T] = {

    val p = Promise[Option[T]]

    publisher.subscribe(new Subscriber[T] {
      var os:Option[Subscription] = None

      override def onError(t: Throwable): Unit = p.failure(t)

      override def onSubscribe(s: Subscription): Unit = {
        os = Some(s)
        s.request(1)
      }

      override def onComplete(): Unit = if (!p.isCompleted) p.success(None)

      override def onNext(t: T): Unit = if (!p.isCompleted) p.success(Some(t))
    })

    RefFuture(p.future).flatMapOpt(RefOpt.apply)
  }

  def collect[T](publisher:Publisher[T])(implicit ec:ExecutionContext):Ref[Seq[T]] = {

    val b = scala.collection.mutable.Buffer.empty[T]
    val p = Promise[Seq[T]]

    publisher.subscribe(new Subscriber[T] {
      var os:Option[Subscription] = None

      override def onError(t: Throwable): Unit = p.failure(t)

      override def onSubscribe(s: Subscription): Unit = {
        println("received subscription")
        os = Some(s)
        s.request(1)
      }

      override def onComplete(): Unit = {
        println("received complete")
        if (!p.isCompleted) p.success(b)
      }

      override def onNext(t: T): Unit = {
        //println(s"received $t")
        b.append(t)
        for { s <- os } s.request(1)
      }

    })

    new RefFuture(p.future)
  }

  implicit class ProcessorOps[T](val p:Publisher[T]) extends AnyVal {

    def mapR[B](f: T => RefOpt[B])(implicit ec:ExecutionContext):Processor[T, B] = new MapR[T, B](p)(f)

    def map[B](f: T => B)(implicit ec:ExecutionContext):Processor[T, B] = new MapR[T, B](p)(f.andThen(RefSome.apply))

  }


}