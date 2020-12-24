package com.wbillingsley.handy.reactivestreams

import scala.concurrent.{ExecutionContext, Future, Promise}

/**
  * Used by our reactive streams classes to sequence operations.
  *
  * The counters and pointers are made thread-safe simply through synchronisation -- these are short operations.
  * Requests to send are sequenced through the flatMap method of Future (monadic sequencing).
  *
  * The sending side should sequence its calls using
  * requestToSend().foreach(operation)
  *
  * The receiving side may signal demand for more items at any time by calling demand(numItems)
  *
  */
class DemandCounter(onDemand: Int => Unit = { _ => })(implicit val ec: ExecutionContext) {

  /**
    * The number of items outstanding in the demand from the client. This should never fall below zero because when it
    * hits zero, requests to send are delayed (asynchronously) using a promise and a future.
    */
  private var demand:Long = 0

  /**
    * Whether this connection has been cancelled
    */
  private var cancelled = false

  /**
    * The number of requests-to-send that have not yet completed.
    */
  private var queued = 0

  /**
    * Whenever there is a request-to-send but no demand, a promise is created that will be fulfilled when new demand
    * from the consumer comes in. We start with a successful promise -- we are not yet waiting to send something.
    */
  private var currentWaitLock:Promise[Boolean] = Promise.successful(false)

  /**
    * The end of the queue of queued requests to send.
    */
  private var currentFuture:Future[Boolean] = currentWaitLock.future

  /**
    * Uses Future.flatMap to ensure strict sequencing of all send requests.
    * Usage example:
    *   for { rts <- requestToSend() } { client.send(item) }
    *
    * @return A future which will complete when the send operation can occur.
    */
  def requestToSend():Future[Boolean] = synchronized {

    /**
      * This should only be called from within currentFuture.flatMap(...) -- it is asynchronous but relies on being
      * strictly sequenced by the monad.
      * @return a Future that either completes immediately if there is demand, or sets a Promise that will be released
      *         when new demand comes in (at which point it will check again)
      */
    def demandTest():Future[Boolean] = synchronized {
      if (cancelled) {
        Future.failed(new InterruptedException("Cancelled by client"))
      } else {
        if (demand > 0) {
          // This request can be fulfilled immediately
          demand = demand - 1
          queued = queued - 1
          Future.successful(cancelled)
        } else if (!currentWaitLock.isCompleted) {
          /*
           * This shouldn't be able to happen. When demand comes in, the waitlock (promise) is fulfilled, and a new
           * promise is not created until demand reaches zero. If this does happen, it perhaps means we've ended up
           * with parallel calls to demandTest, rather than only being monadically sequenced.
           */
          Future.failed(new IllegalStateException("Wait lock was not completed, but demand > 0"))
        } else {
          // Not now; when some new demand comes in we will check again
          currentWaitLock = Promise[Boolean]
          currentWaitLock.future.flatMap(_ => demandTest())
        }
      }
    }

    // TODO: Check if we are over a maximum buffer (if queued > some max) in which case fail immediately

    // Queue a demand test, which might even run immediately (if the queue is not already waiting on a promise)
    queued += 1
    currentFuture = currentFuture.flatMap(_ => demandTest())
    currentFuture
  }

  /**
    * Called at any time to indicate that the client has demanded more
    */
  def demand(n:Long):Unit = synchronized {
    if (n > 0) {
      demand += n

      // If there is a current wait-lock (a promise watiing to be fulfilled), clear it
      if (!currentWaitLock.isCompleted) {
        currentWaitLock.success(cancelled)
      }
    }

    // call any event-handler we've been asked to call (eg this might pass on the demand up the reactive-stream)
    onDemand
  }

  /**
    * Cancels this connection in such a way that requests to send will eventually fail with an InterruptedException
    */
  def cancel():Unit = synchronized {
    cancelled = true

    // Immediately fail all queued requests to send
    if (!currentWaitLock.isCompleted) {
      currentWaitLock.failure(new InterruptedException("Cancelled by client"))
    }
  }

}
