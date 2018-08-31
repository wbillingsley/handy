package com.wbillingsley.handy.reactivestreams

import com.wbillingsley.handy.{RefFailed, RefNone, Ref}
import org.reactivestreams.{Subscriber, Subscription, Processor, Publisher}

import scala.concurrent.{Future, ExecutionContext}

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
      processed <- f(t).withFilter(identity) orIfNone {
        // If processed is false, time to stop
        s.cancel(); RefNone
      } recoverWith {
        case x => onError(x); RefFailed(x)
      }
      pushed <- Future.sequence(outbound.map(_.push(t)))
    } {
      s.request(1)
    }
  }

}