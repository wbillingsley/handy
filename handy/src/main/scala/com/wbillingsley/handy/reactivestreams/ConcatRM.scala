package com.wbillingsley.handy.reactivestreams

import com.wbillingsley.handy.{RefMany, RefNone, RefFailed, Ref}
import org.reactivestreams.{Subscriber, Subscription, Processor, Publisher}

import scala.concurrent.{ExecutionContext, Promise, Future}

/**
  * A Processor that (possibly asynchronously) processes each item.
  * RefItself(item) => item will be passed on
  * RefNone => item will be skipped
  * RefFailed(x) =>
  */
class ConcatRM[T](pubs:RefMany[Publisher[T]])(implicit ec:ExecutionContext) extends Processor[T, T] {

  var inbound:Option[Subscription] = None
  var outbound:Option[DCSubscription[T]] = None

  var completedCurrent = Promise.successful(false)

  /**
    * Subscribe to the next one, or complete
    */
  private def start() = {
    pubs.foldLeft[Future[Boolean]](Future.successful(false))({ case (last, next) =>
      last.flatMap { ready =>
        next.subscribe(this)
        synchronized {
          completedCurrent = Promise[Boolean]
        }
        completedCurrent.future
      }
    }).toFuture.flatMap(identity _).map({ done =>
      for { o <- outbound } { o.pushComplete() }
      done
    }).recoverWith { case x =>
      for { o <- outbound } { o.pushError(x) }
      Future.failed(x)
    }
  }

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
      start()

      for { sq <- outbound } {
        sq.subscriber.onSubscribe(sq)
      }
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
    completedCurrent.success(false)
  }

  override def onNext(t: T): Unit = {
    for {
      i <- inbound
      s <- outbound
      sent <- s.push(t)
    } {
      i.request(1)
    }
  }

}
