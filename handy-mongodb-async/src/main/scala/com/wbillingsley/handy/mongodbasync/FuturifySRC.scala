package com.wbillingsley.handy.mongodbasync

import com.mongodb.async.SingleResultCallback
import scala.concurrent.{Future, Promise}

class FuturifySRC[T](nullIsEmpty:Boolean = true) {
  private val p = Promise[T]

  val callback = new SingleResultCallback[T] {
    override def onResult(t: T, throwable: Throwable): Unit = {
      if (throwable != null) {
        p.failure(throwable)
      } else {
        if (nullIsEmpty && t == null) {
          p.failure(new NoSuchElementException())
        } else {
          p.success(t)
        }
      }
    }
  }

  def future = p.future
}

object FuturifySRC {

  def apply[T](block: SingleResultCallback[T] => Unit):Future[T] = {
    val f = new FuturifySRC[T]
    block(f.callback)
    f.future
  }

  def void(block: SingleResultCallback[Void] => Unit):Future[Void] = {
    val f = new FuturifySRC[Void](false)
    block(f.callback)
    f.future
  }

}
