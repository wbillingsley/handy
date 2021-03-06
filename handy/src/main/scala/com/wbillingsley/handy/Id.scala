package com.wbillingsley.handy

import scala.concurrent.Future

/**
 * The ID of an item
 *
 * @param id
 * @tparam T covariant: the Id of a horse is an Id of a mammal
 * @tparam K invariant.
 */
case class Id[+T,K](id: K) {

  def lookUp[TT >: T](implicit lu: LookUp[TT,K]) = lu.one(this)

  def lazily[TT >: T](implicit lu: LookUp[TT,K]) = LazyId(id, lu.one[K])

}

case class UntypedId[K](id: K) {
  def of[T] = new Id[T,K](id)
}

object Id {
  def apply[K](id:K) = UntypedId(id)

  case class LookUpPair[T,K](id:Id[T,K], lu:LookUp[T,K])

  implicit class AsId[K](val k:K) extends AnyVal {
    def asId[T]:Id[T,K] = Id[T,K](k)
  }

  implicit class OptLookup[+T,K](val o:Option[Id[T,K]]) extends AnyVal {
    def lookUp[TT >: T](implicit lu: LookUp[TT,K]):RefOpt[TT] = {
      RefOpt(o).flatMap(_.lookUp(lu))      
    }

    def lazily[TT >: T](implicit lu: LookUp[TT,K]):RefOpt[TT] = {
      RefOpt(o).flatMapOne(_.lazily(lu))
    }
  }
}