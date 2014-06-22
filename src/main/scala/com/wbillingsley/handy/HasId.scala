package com.wbillingsley.handy

/**
 * Indicates that an object has an `id` method that will return an `Id` to it.
 */
trait HasId[+T, K] {
  def id:Id[T, K]
}

trait HasStringId[+T] extends HasId[T, String]

/**
 * Knows how to get an `Id` from an object
 */
trait GetsId[-T, K] {
  def getId[TT <: T](obj: TT): Option[Id[TT, K]]

  def canonical[TT <: T](o:Any):Option[Id[TT,K]]
}

object HasStringId {

  implicit object getsStringId extends GetsId[HasStringId[_], String] {
    def getId[T <: HasStringId[_]](obj: T) = {
      import Id._

      val k = obj.id.id
      Some(k.asId[T])
    }

    def canonical[T <: HasStringId[_]](id:Any) = id match {
      case s:String => Some(Id[T,String](s))
      case _ => None
    }
  }

}