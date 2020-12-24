package com.wbillingsley.handy.reactivestreams

import org.reactivestreams.{Subscriber, Subscription}

import scala.concurrent.{ExecutionContext, Future}

/**
  * A subscription that queues up futures (just by scheduling them on-complete of the previous
  * one) to buffer its output. This keeps it very small, and leaves it to the execution
  * service to deal with actual queues.
  *
  * Note - as the queue works asynchronously by keeping a chain of futures, the number of
  * Futures being chained is not limited (it could be possible to overflow memory)
  */
class DCSubscription[T](val subscriber:Subscriber[_ >: T])(implicit val ec: ExecutionContext) extends Subscription {

  val demandCounter = new DemandCounter()

  // The last item that was sequenced. This ensures that items are sent in order.
  private var last:Future[Any] = Future.successful(true)

  override def cancel(): Unit = demandCounter.cancel()

  /**
    * Called by the subscriber to register demand for more items
    */
  override def request(n: Long): Unit = {
    demandCounter.demand(n)
  }

  /**
   * Pushes are a chain of Futures. We keep the most recent of these, and when 
   * a new message is sequenced add it to the chain.
   */
  private def sequencePush[U](f: => U):Future[U] = synchronized {
    val result = last.flatMap { _ => 
      demandCounter.requestToSend().map(_ => f).recoverWith {
        case x => {
          demandCounter.cancel()
          subscriber.onError(x)
          Future.failed(x)
        }
      }    
    }
    last = result
    result
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

