package com.wbillingsley.handy.reactivestreams

import com.wbillingsley.handy.{Ref, RefFailed, RefNone, RefOptFailed}
import org.reactivestreams.{Processor, Publisher, Subscriber, Subscription}

import scala.concurrent.{ExecutionContext, Future}

class TakeWhileR[T](pub:Publisher[T])(f: T => Ref[Boolean])(implicit val ec: ExecutionContext) extends Processor[T, T] {

  import scala.collection.mutable

  var subscribed:Boolean = false
  var inbound:Option[Subscription] = None
  var outbound:mutable.Buffer[DCSubscription[T]] = mutable.Buffer.empty

  override def subscribe(s: Subscriber[_ >: T]): Unit = synchronized {
    val sq = new DCSubscription[T](s)
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
      processed <- f(t).optional.withFilter(identity) orElse {
        // If processed is false, time to stop
        s.cancel(); RefNone
      } recoverWith {
        case x => onError(x); RefOptFailed(x)
      }
      pushed <- Future.sequence(outbound.map(_.push(t)))
    } {
      s.request(1)
    }
  }

}