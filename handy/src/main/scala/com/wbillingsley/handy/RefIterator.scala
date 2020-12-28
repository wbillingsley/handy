package com.wbillingsley.handy

import com.wbillingsley.handy.Trampolines._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

/**
  * A possibly-asynchronous immutable iterator across a RefMany[T].
  * 
  * Note the similarity to a List. `item` is analagous to `head`, except that it is permitted to be absent (or to fail).
  * `next` is analagous to tail
  *
  * @tparam T
  */
trait RefIterator[+T] {

  /**
    * The item pointed to.
    * To allow for some implementations to iterate across empty elements, and for some implementations to return the
    * iterator immediately but the item asynchronously, this is a RefOpt.
     * @return
    */
  def item:RefOpt[T]

  /**
    * The next item. Possibly asynchronous, and it's possible we've come to the end of the stream (in which case there
    * would be none)
    * @return
    */
  def next:RefOpt[RefIterator[T]]

}

object RefIterator {

  import scala.annotation.tailrec

  case class TooManyIterations(max:Int) extends Throwable

  def asyncProcess[A, B](start:B)(iterator:RefIterator[A], processor:RefConsumer[A, B])
                        (using ec:ExecutionContext):Ref[B] = {
    // We use Future as a trampoline - this should cause the computation to occur on ec, and ensure the recursion
    // does not produce a stack overflow, because we are returning RefFutures that will do processing, rather than
    // directly recursing
    val done:Ref[Unit] = Future.successful(()).toRef

    for {
      now <- done
      (result, optNextProc) <- processor.process(start, iterator.item)
      recurse:B <- optNextProc match {
        case Some(nextProc) =>
          for
            optNextIt <- iterator.next.option
            rec <- optNextIt match {
              case Some(nextIt) => process(result)(nextIt, nextProc)
              case _ => result.itself
            }
          yield rec
        case None => result.itself
      }
    } yield recurse
  }

  /**
    * Runs a processor synchronously, if the Refs are synchronous and it's not too many iterations.
    * Diverts to asynchronous processing as soon as it sees something that isn't a RefItself, RefSome, RefNone.
    * 
    * TODO: Name is problematic as it suggests mutation escapes the function (whereas it's just internally mutable)
    *
    * @param ec An execution context to use if it goes async.
    */
  def mutableProcess[A, B](start:B)
                          (iterator:RefIterator[A], processor:RefConsumer[A, B])
                          (using ec:ExecutionContext):Ref[B] = {
    var current = iterator
    var proc = processor
    var res = start
    val continue = true

    while (continue) {

      processor.process(res, current.item) match {
        case RefItself((outcome, Some(nextProcessor))) =>
          res = outcome
          proc = nextProcessor
          current.next match {
            case RefSome(nextIt) =>
              current = nextIt
            case RefNone =>
              return RefItself(res)
            case RefOptFailed(x) =>
              return RefFailed(x)
            case _ =>
              return asyncProcess(res)(current, proc)
          }
        case RefItself((outcome, None)) =>
          return RefItself(outcome)
        case RefFailed(x) =>
          return RefFailed(x)
        case _ =>
          return asyncProcess(res)(current, proc)
      }
    }

    // If we haven't completed at this point, we go asynchronous
    asyncProcess(res)(current, proc)
  }

  def process[A, B](start:B)(iterator:RefIterator[A], processor:RefConsumer[A, B])(using ec:ExecutionContext):Ref[B] =
    mutableProcess(start)(iterator, processor)(using ec)

}

/**
  * Consumes a RefIterator[T].
  * @tparam T
  */
trait RefConsumer[-T, R] {

  def process(b: R, a: RefOpt[T]): Ref[(R, Option[RefConsumer[T, R]])]

}

object RefConsumer {

  def foldLeft[T, R](rm:RefMany[T])(start:R)(f: (R, T) => R)(using ec:ExecutionContext):Ref[R] = rm.iterator.flatMap({ it =>
    val consumer = new RefConsumer[T, R] {
      var res = start

      override def process(b: R, a: RefOpt[T]): Ref[(R, Option[RefConsumer[T, R]])] = {
        (for 
          aa <- a 
        yield {
          res = f(b, aa)  
          (res, Some(this))
        }) orElse (res, Some(this)).itself
      }
    }

    RefIterator.process(start)(it, consumer)
  }) orElse start.itself

}

/*
enum Iteratee[T, R] {
  case Done(item: R) extends Iteratee[T, R]
  case Continue(item: R, f: RefOpt[T] => Ref[Iteratee[T, R]]) extends Iteratee[T, R]
}

object Iteratee {
  object EOF
  type Input[T] = RefOpt[T] | EOF.type
}
*/
