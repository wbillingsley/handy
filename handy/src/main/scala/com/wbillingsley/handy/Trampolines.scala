package com.wbillingsley.handy

import scala.annotation.tailrec
import scala.collection.mutable

object Trampolines {


  sealed trait Trampoline[T] {
    @tailrec
    final def result: T = this match {
      case Done(v) => v
      case Continue(comp) => comp().result
    }
  }

  case class Done[T](value: T) extends Trampoline[T]

  case class Continue[T](computation: () => Trampoline[T]) extends Trampoline[T]

}
