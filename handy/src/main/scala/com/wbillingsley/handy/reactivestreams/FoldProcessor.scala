package com.wbillingsley.handy.reactivestreams

import com.wbillingsley.handy.{RefFailed, RefNone, Ref}
import org.reactivestreams.{Subscriber, Subscription, Processor, Publisher}

import scala.concurrent.{ExecutionContext, Promise, Future}


/**
  * A Processor that (possibly asynchronously) processes each item.
  * RefItself(item) => item will be passed on
  * RefNone => item will be skipped
  * RefFailed(x) =>
  */
class FoldProcessor[T, R](pub:Publisher[T])(initial: => R)(each: (R, T) => R)(implicit ec:ExecutionContext) extends Processor[T, R] {

  var inbound:Option[Subscription] = None
  var outbound:Option[DCSubscription[R]] = None

  private var last = initial

  private val done = Promise[R]

  private var started = false

  private def start() = synchronized {
    if (!started) {
      started = true
      pub.subscribe(this)
    }
  }

  override def subscribe(s: Subscriber[_ >: R]): Unit = {
    synchronized {
      if (outbound.nonEmpty) {
        s.onError(new IllegalStateException("This publisher does not support multiple subscription"))
      } else {
        val sq = new DCSubscription[R](s)
        outbound = Some(sq)
      }
    }

    start()
  }

  override def onError(t: Throwable): Unit = {
    for { sq <- outbound } {
      sq.pushError(t)
    }
    done.failure(t)
  }

  override def onSubscribe(s: Subscription): Unit = {
    inbound = Some(s)
    for { sq <- outbound } {
      sq.subscriber.onSubscribe(sq)
    }
    s.request(1)
  }

  override def onComplete(): Unit = {
    done.success(last)
    for { sq <- outbound } {
      sq.push(last)
      sq.pushComplete()
    }
  }

  def toFuture = {
    start()
    done.future
  }

  override def onNext(t: T): Unit = {
    last = each(last, t)
    for { s <- inbound } s.request(1)
  }

}

