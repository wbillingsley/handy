package com.wbillingsley.handy.reactivestreams

import com.wbillingsley.handy.{RefItself, RefMany}
import org.reactivestreams.Publisher

import scala.concurrent.ExecutionContext


implicit class PubOps[T](val rm:Publisher[T]) extends AnyVal {
  def toRefMany(implicit ec:ExecutionContext):RefMany[T] = new RefPublisher(rm)
}

implicit class RMStreamOps[T](val rm:RefMany[T]) extends AnyVal {

  def takeWhile(func: (T) => Boolean)(implicit ec:ExecutionContext): RefMany[T] = {
    val pub = new RMPublisher[T](rm)
    val tw  = new TakeWhileR[T](pub)(func.andThen(RefItself.apply))
    new RefPublisher[T](tw)
  }

}


class RefPublisherOps {

}
