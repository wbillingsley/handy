package com.wbillingsley.handy

case class Ids[T, K](ids: Seq[K]) {

  import Ids._

  def lookUp(implicit lu: LookUpMany[T,K]) = lu(this)

}

case class UntypedIds[K](id: K) {
  def of[T] = new Id[T,K](id)
}

object Ids {
  def apply[K](ids:Seq[K]) = UntypedIds(ids)

  type LookUpMany[T,K] = Ids[T,K] => RefMany[T]

  case class LookUpPair[T,K](ids:Ids[T,K], lu:LookUpMany[T,K])

  implicit class AsIds[K](val k:Seq[K]) extends AnyVal {
    def asIds[T] = Ids[T,K](k)
  }

}

