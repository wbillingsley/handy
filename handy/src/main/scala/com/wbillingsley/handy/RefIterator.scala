package com.wbillingsley.handy

import com.wbillingsley.handy.Trampolines._

import scala.concurrent.{Future, ExecutionContext}

/**
  * A possibly-asynchronous iterator across a RefMany[T]
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

  /*
  def tv[A, B](start:B)(iterator:RefIterator[A], processor:RefConsumer[A, B]):Ref[B] = {
    
    def inner(start:B, item:Option[A], iterator:RefIterator[A], processor:RefConsumer[A, B]):Trampoline[Ref[B]] = {
      item match {
        case None => Done(start.itself)
        case Some(i) => Continue { () =>
          
          processor.process(start, i) flatMap { (res, optNextConsumer) =>

            optNextConsumer match {
              case None => 
                Done(res.itself)
              
              case Some(nextProc) =>
                Continue {
                  val nextPair: RefOpt[(A, RefIterator[A])] = for
                  nextIter <- iterator.next
                  nextItem <- nextIter.item
                    yield (nextItem, nextIter)

                  nextPair.option.flatMap {
                    case Some(item, iter) =>
                      inner(res, Some(item), iter, processor)
                    case _ =>
                      inner(res, None, iterator, processor)
                  }
                }
            }
            
          }
          
        }
          
      }

    }
    
    
    
    
  
  }


*/
   /*
  def trampolineProcess[A, B](start:B)(iterator:RefIterator[A], processor:RefConsumer[A, B]):Ref[B] = {
    (AsyncTrampoline.AsyncContinue { () => 
      iterator.item.option.flatMapOne {
        // If there's no item, we're done with the result 
        case None => AsyncTrampoline.Done(start).itself
          
        case Some(i) =>
          processor.process(start, i).flatMapOne { _.match
            case (result, None) => AsyncTrampoline.Done(result).itself:Ref[AsyncTrampoline[B]]
            case (result, Some(p)) => 
              iterator.next.option.flatMapOne {
                case None => AsyncTrampoline.Done(result).itself:Ref[AsyncTrampoline[B]]
                case Some(it) => 
                  AsyncTrampoline.AsyncContinue({
                    () => trampolineProcess(result)(it, p).map(x => AsyncTrampoline.Done(x)):Ref[AsyncTrampoline[B]]
                  }).itself
              }
          }
      }
    }).result
  }*/
  
  /*
  def processVia[A, B](start:B)(iterator:RefIterator[A], processor:RefConsumer[A, B]):Ref[B] = {
    val ro = for 
      item <- iterator.item
      (result, optNext) <- processor.process(start, item)
      recurse <- optNext match {
        case Some(next) =>
          iterator.next.flatMap(it => processVia(result)(it, next)) orElse result.itself
        case None => result.itself
      }
    yield recurse
    
    ro orElse start.itself
  }*/

  case class TooManyIterations(max:Int) extends Throwable
  
  def asyncProcess[A, B](start:B)(iterator:RefIterator[A], processor:RefConsumer[A, B],
                                  fromIteration:Int = 0, maxIterations:Option[Int]=None)
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
  
  def process(b:R, a:RefOpt[T]):Ref[(R, Option[RefConsumer[T, R]])]
  
}

object RefConsumer {
  
  def foldRight[T, R](rm:RefMany[T])(start:R)(f: (T, R) => R):Ref[R] = {
    ???
    
  }
  
}