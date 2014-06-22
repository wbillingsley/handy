package com.wbillingsley.handy

trait LookUp[T, -K] {

  def one[KK <: K](r:Id[T,KK]): Ref[T]

  def many[KK <: K](r:Ids[T,KK]): RefMany[T]

}

object LookUp {

  def empty[T, K] = new LookUp[T, K] {
    def one[KK <: K](r:Id[T, KK]) = RefNone

    def many[KK <: K](r:Ids[T, KK]) = RefNone
  }

  def fails[T, K](msg:String) = new LookUp[T, K] {
    def one[KK <: K](r:Id[T, KK]) = new RefFailed(new IllegalStateException(msg))

    def many[KK <: K](r:Ids[T, KK]) = new RefFailed(new IllegalStateException(msg))
  }

}

