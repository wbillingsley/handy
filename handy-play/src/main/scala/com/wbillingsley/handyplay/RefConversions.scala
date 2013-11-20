package com.wbillingsley.handyplay

import com.wbillingsley.handy._
import play.api.libs.iteratee._
import scala.concurrent.Future

object RefConversions {

  implicit class EnumerateRefMany[T](val rm: RefMany[T]) {

    def enumerate = new Enumerator[T] {

      def apply[A](it: Iteratee[T, A]) = {
        
        val res = rm.fold(it)((it, el) => Iteratee.flatten(it.feed(Input.El(el))))
        val p = scala.concurrent.promise[Iteratee[T, A]]
        for (r <- res) p.success(r)
        p.future
        
      }
      
    }

  }
  
  
  implicit class EnumerateRef[T](val r:Ref[T]) {

    def enumerate = {
      new Enumerator[T] {
        def apply[A](it: Iteratee[T, A]) = {
          
          import RefFuture.executionContext

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