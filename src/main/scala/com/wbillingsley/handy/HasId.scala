package com.wbillingsley.handy

trait HasId[K] {
  
  def id:K

}

trait GetsId[T, K] {
  
  def getId(obj: T): Option[K]
  
  def canonical(key:Any): Option[K]
  
}

/**
 * In places where we want to use an implicit GetsId, but still allow the call
 * if there isn't one (such as EqualsById's equals method), this is used as a
 * default for the implicit argument.  Essentially telling the compiler
 * "If there is no implicit GetsId, it has no id". 
 */
object GetsNoId extends GetsId[Any, Nothing] {
  def getId(obj:Any) = None
  
  def canonical(key:Any) = None
}
