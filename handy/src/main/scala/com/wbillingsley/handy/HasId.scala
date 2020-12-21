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
trait GetsId[T, Key <: Id[T, _]] {
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
  def canonical[TT <: T](o:Any):Option[Key]
}
