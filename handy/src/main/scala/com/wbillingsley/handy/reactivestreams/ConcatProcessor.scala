package com.wbillingsley.handy.reactivestreams

import com.wbillingsley.handy.{RefNone, RefFailed, Ref}
import org.reactivestreams.{Subscriber, Subscription, Processor, Publisher}

import scala.concurrent.{ExecutionContext, Promise, Future}

/**
  * A Processor that (possibly asynchronously) processes each item.
  * RefItself(item) => item will be passed on
  * RefNone => item will be skipped
  * RefFailed(x) =>
  */
class ConcatProcessor[T](pubs:Publisher[Publisher[T]])(implicit ec:ExecutionContext) extends Processor[Publisher[T], T] {

  var inbound:Option[Subscription] = None
  var outbound:Option[DCSubscription[T]] = None

  override def subscribe(s: Subscriber[_ >: T]): Unit = {
    var firstSub = true
    synchronized {
      if (outbound.nonEmpty) {
        s.onError(new IllegalStateException("This publisher does not support multiple subscription"))
        firstSub = false
      } else {
        val sq = new DCSubscription[T](s)
        outbound = Some(sq)
      }
    }

    if (firstSub) {
      pubs.subscribe(this)
      for { subscr <- outbound } s.onSubscribe(subscr)
    }
  }

  override def onError(t: Throwable): Unit = {
    for { sq <- outbound } {
      sq.pushError(t)
    }
  }

  override def onSubscribe(s: Subscription): Unit = {
    inbound = Some(s)
    s.request(1)
  }

  override def onComplete(): Unit = synchronized {
    inbound = None
  }

  override def onNext(pub: Publisher[T]): Unit = {
    pub.subscribe(new Subscriber[T] {
      var os:Option[Subscription] = None

      override def onError(t: Throwable): Unit = for { sq <- outbound } {
        sq.pushError(t)
      }

      override def onSubscribe(s: Subscription): Unit = {
        os = Some(s)
        s.request(1)
      }

      override def onComplete(): Unit = {
        inbound match {
          case Some(i) => i.request(1)
          case None => for { s <- outbound } s.pushComplete()
        }
      }

      override def onNext(t: T): Unit = {
        for { sq <- outbound } {
          sq.push(t)
          for { s <- os } s.request(1)
        }
      }
    })
  }

}
