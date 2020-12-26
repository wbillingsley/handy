package com.wbillingsley.handy

import scala.annotation.tailrec
import scala.collection.mutable

/* - not used, but left as comment for note.

enum AsyncTrampoline[T] {
  case Done[T](v: T) extends AsyncTrampoline[T]
  case SyncContinue[T](f: () => AsyncTrampoline[T]) extends AsyncTrampoline[T]
  case AsyncContinue[T](val f: () => Ref[AsyncTrampoline[T]]) extends AsyncTrampoline[T] 
  
  def flatten(a:AsyncContinue[T]) = {
    a.f().flatMap(_.result)
  }
  
  @tailrec final def result:Ref[T] = this match {
    case Done(v) => v.itself
    case SyncContinue(f) => f().result
    case AsyncContinue(f) => flatten(AsyncContinue(f))
  }
  
}

*/

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
