package com.wbillingsley.handy

trait HasId[K] {
  
  def id:K

}

trait GetsId[-T, K] {
  
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


import scala.util.Try

trait HasStringId extends HasId[String] {
  
}

object HasStringId {
  
  implicit object GetsStringId extends GetsId[HasStringId, String] {
    
    var pf:PartialFunction[Any, Option[String]] = { 
      case s:String => Some(s)
      case _ => None 
    }
    
	def getId(obj:HasStringId) = Some(obj.id)
  
	def canonical(key:Any) = pf(key) 
  }   
}