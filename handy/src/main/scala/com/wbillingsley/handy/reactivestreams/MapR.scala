package com.wbillingsley.handy.reactivestreams

import com.wbillingsley.handy._
import org.reactivestreams.{Processor, Publisher, Subscriber, Subscription}

import scala.concurrent.{ExecutionContext, Future}

/**
  * A Processor that (possibly asynchronously) processes each item.
  * RefSome(item) => item will be passed on
  * RefNone => item will be skipped
  * RefFailed(x) =>
  */
class MapR[T, R](pub:Publisher[T])(f: T => RefOpt[R])(implicit val ec: ExecutionContext) extends Processor[T, R] {

  import scala.collection.mutable

  var subscribed:Boolean = false
  var inbound:Option[Subscription] = None
  var outbound:mutable.Buffer[DCSubscription[R]] = mutable.Buffer.empty

  override def subscribe(s: Subscriber[_ >: R]): Unit = synchronized {
    val sq = new DCSubscription[R](s)
    outbound.append(sq)
    if (!subscribed) {
      subscribed = true
      pub.subscribe(this)
    }
    s.onSubscribe(sq)
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

  override def onComplete(): Unit = {
    for { sq <- outbound } {
      sq.pushComplete()
    }
  }

  override def onNext(t: T): Unit = {
    for {
      s <- inbound
      processed <- f(t).orElse[R] {
        // If this one has been skipped, request another
        s.request(1); RefNone
      } recoverWith[R] {
        case x => onError(x); RefOptFailed(x)
      }
      pushed <- Future.sequence(outbound.map(_.push(processed)))
    } {
      s.request(1)
    }
  }

}


