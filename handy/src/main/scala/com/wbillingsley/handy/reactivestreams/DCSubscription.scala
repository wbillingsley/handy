package com.wbillingsley.handy.reactivestreams

import org.reactivestreams.{Subscription, Subscriber}

import scala.concurrent.{ExecutionContext, Future}

/**
  * A subscription that queues up futures (just by scheduling them on-complete of the previous
  * one) to buffer its output. This keeps it very small, and leaves it to the execution
  * service to deal with actual queues.
  */
class DCSubscription[T](val subscriber:Subscriber[_ >: T])(implicit val ec: ExecutionContext) extends Subscription {

  val demandCounter = new DemandCounter()

  override def cancel(): Unit = demandCounter.cancel()

  /**
    * Called by the subscriber to register demand for more items
    */
  override def request(n: Long): Unit = {
    demandCounter.demand(n)
  }

  private def sequencePush[U](f: => U):Future[U] = {
    demandCounter.requestToSend().map(_ => f).recoverWith {
      case x => {
        demandCounter.cancel()
        subscriber.onError(x)
        Future.failed(x)
      }
    }
  }

  def pushError(t:Throwable) = sequencePush {
    demandCounter.cancel()
    subscriber.onError(t)
  }

  def pushComplete() = {
    sequencePush {
      demandCounter.cancel()
      subscriber.onComplete()
    }
  }

  def push(item:T) = {
    sequencePush {
      subscriber.onNext(item)
    }
  }

}

