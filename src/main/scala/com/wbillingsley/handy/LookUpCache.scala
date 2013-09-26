package com.wbillingsley.handy

import scala.collection.concurrent.TrieMap

/**
 * A mutable cache for Refs
 */
class LookUpCache {
  
  val cache = TrieMap.empty[Any, List[(Class[_], Ref[_])]]
  
  def cacheLookup[T, K, KK](rbi: RefById[T, K])(implicit g: GetsId[T, KK]):Ref[T] = {    
    rbi.getId match {
      case Some(id) => { 
        find(rbi.clazz, id).getOrElse({
          val l = rbi.lookUp
          remember(rbi.clazz, l)
          l 
        })
      }
      case None => rbi
    }
  }
  
  def cacheLookup[T, K, KK](li: LazyId[T, K])(implicit g: GetsId[T, KK]):Ref[T] = {    
    li.getId match {
      case Some(id) => { 
        find(li.rbi.clazz, id).getOrElse({
          remember(li.rbi.clazz, li)
          li
        })
      }
      case None => li
    }
  }  
  
  private def find[T](clazz:Class[T], id:Any):Option[Ref[T]] = {    
    val list = cache.getOrElse(id, Nil)        
    val tup = list.find { clazz isAssignableFrom _._1 }
    tup map { _._2.asInstanceOf[Ref[T]] }
  }  
  
  def remember[T, KK](clazz: Class[_], r:Ref[T])(implicit g: GetsId[T, KK]) = {
    r.getId match {
      case Some(id) => {
        import scala.language.existentials
        
        val l = (clazz, r) :: cache.getOrElse(id, Nil)
        cache.put(id, l)
        r
      }
      case None => r
    }
  }
  
  def apply[T, TT >: T, KK](r: Ref[T], clazz:Class[TT])(implicit g: GetsId[T, KK]):Ref[T] = {
    r match {
      case rbi:RefById[T, _] => cacheLookup(rbi)
      case li:LazyId[T, _] => cacheLookup(li)
      case r:Ref[T] => remember(clazz, r)
    }
  }  

  def apply[T, TT >: T, K, KK](rbi: RefById[T, K])(implicit g: GetsId[T, KK]):Ref[T] = {
	cacheLookup(rbi)
  }

  def apply[T, TT >: T, K, KK](li: LazyId[T, K])(implicit g: GetsId[T, KK]):Ref[T] = {
	cacheLookup(li)
  }
  
  def apply[T, KK](ri: RefItself[T])(implicit g: GetsId[T, KK]):Ref[T] = {
	remember(ri.item.getClass, ri)
  }
  
}