package com.wbillingsley.handyplay

import com.wbillingsley.handy._
import Ref._
import play.api.libs.iteratee._
import scala.concurrent.{ExecutionContext, Future}

object RefConversions {

  implicit class EnumerateRefMany[T](val rm: RefMany[T]) {

    def enumerate(implicit executionContext:ExecutionContext):Enumerator[T] = rm match {
      case re:RefEnumerator[T] => re.enumerator
      case rei:RefEnumIter[T] => rei.enumerator.flatMap(trav => Enumerator.enumerate(trav))

      case _ => new Enumerator[T] {
        def apply[A](it: Iteratee[T, A]) = {
          import com.wbillingsley.handy.Ref._
          val res = rm.fold(it) { (it, el) => Iteratee.flatten(it.feed(Input.El(el))) }
          res.toFutOpt.map(_.getOrElse(it))(RefFuture.executionContext)
        }
      }
    }
    
    def enumerateR(implicit executionContext:ExecutionContext):Ref[Enumerator[T]] = rm whenReady { _.enumerate }
  }
  
  
  implicit class EnumerateRef[T](val r:Ref[T]) {

    def enumerate(implicit executionContext:ExecutionContext) = {
      new Enumerator[T] {
        def apply[A](it: Iteratee[T, A]) = {

          r match {
            case RefItself(item) => {
              it.pureFold {
                case Step.Cont(k) => k(Input.El(item))
                case _ => it
              }
            }
            case RefNone => Future.successful(it)
            case RefFailed(exc) => {
              it.pureFold {
                case Step.Cont(k) => Step.Error(exc.getMessage, Input.Empty).it
                case _ => it
              }
            }
            case _ => {
              it.fold {
                case Step.Cont(k) => {
                  val p = scala.concurrent.promise[Iteratee[T, A]]
                  r.onComplete(onSuccess={ el =>
                    p.success(k(Input.El(el)))
                  }, 
                  onNone = {
                    p.success(it)
                  }, 
                  onFail= { exc =>
                    p.success(Step.Error(exc.getMessage, Input.Empty).it)
                  })
                  p.future
                }
                case _ => Future.successful(it)
              }
            }
          }
        }
      }
    }

  }
  
}