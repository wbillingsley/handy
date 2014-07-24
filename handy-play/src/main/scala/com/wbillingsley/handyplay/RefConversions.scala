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
          val res = rm.fold(it) { (it, el) => Iteratee.flatten(it.feed(Input.El(el))) }
          res.toFutOpt.map(_.getOrElse(it))(executionContext)
        }
      }
    }
    
    def enumerateR(implicit executionContext:ExecutionContext):Ref[Enumerator[T]] = rm whenReady { _.enumerate }

    /**
     * Turns the RefMany into an Enumerator, and pushes it through an Enumeratee.
     * This allows for things like <code>take(n)</code>, by applying an appropriate Enumeratee.
     */
    def through[B](e:Enumeratee[T, B])(implicit executionContext:ExecutionContext):RefMany[B] = {
      new RefEnumerator[B](enumerate through e)
    }
  }
  
  
  implicit class EnumerateRef[T](val r:Ref[T]) {

    def enumerate(implicit executionContext:ExecutionContext):Enumerator[T] = {
      val futureEnum = for {
        opt <- r.toFutOpt
      } yield opt match {
        case Some(item) => Enumerator[T](item)
        case None => Enumerator.empty[T]
      }
      Enumerator.flatten(futureEnum)
    }

  }
  
}