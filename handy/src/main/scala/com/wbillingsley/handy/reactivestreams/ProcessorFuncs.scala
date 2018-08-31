package com.wbillingsley.handy.reactivestreams

import com.wbillingsley.handy._
import org.reactivestreams.{Subscriber, Subscription, Processor, Publisher}

import scala.concurrent.{ExecutionContext, Promise, Future}

/**
  * A Processor that (possibly asynchronously) processes each item.
  * RefItself(item) => item will be passed on
  * RefNone => item will be skipped
  * RefFailed(x) =>
  */
object ProcessorFuncs {

  def headR[T](publisher: Publisher[T])(implicit ec:ExecutionContext) = {

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

    for {
      o <- new RefFuture(p.future)
      item <- Ref(o)
    } yield item
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
        if (!p.isCompleted) p.success(b.toSeq)
      }

      override def onNext(t: T): Unit = {
        println(s"received $t")
        b.append(t)
        for { s <- os } s.request(1)
      }

    })

    new RefFuture(p.future)
  }

  implicit class ProcessorOps[T](val p:Publisher[T]) extends AnyVal {

    def mapR[B](f: T => Ref[B])(implicit ec:ExecutionContext):Processor[T, B] = new MapR[T, B](p)(f)

    def map[B](f: T => B)(implicit ec:ExecutionContext):Processor[T, B] = new MapR[T, B](p)(f.andThen(RefItself.apply))

  }


}