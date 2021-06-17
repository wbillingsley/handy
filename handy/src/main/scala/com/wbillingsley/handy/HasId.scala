package com.wbillingsley.handy

/**
 * Indicates that an object has an `id` method that will return an `Id` to it.
 */
trait HasId[+K] {
  def id:K
}

inline def autoGetsId[T <: HasId[K], K] = new GetsId[T, K] {
  def getId[TT <: T](obj: TT):Option[K] = Some(obj.id)

  def canonical(o: Any): Option[K] = o match {
    case x:K => Some(x)
    case _ => None
  }
}

/**
 * Knows how to get an `Id` from an object
 */
trait GetsId[-T, +Key] {
  def getId[TT <: T](obj: TT): Option[Key]

  /**
    * Given an object purporting to be the right sort of key, return Some(key) if it is, or None if it isn't.
    *
    * Usually, this will be a simple pattern match, e.g.:
    *
    * o match {
    *   case a:OrangeId => Some(a)
    *   case _ => None
    * }
    */
  def canonical(o:Any):Option[Key]
}
