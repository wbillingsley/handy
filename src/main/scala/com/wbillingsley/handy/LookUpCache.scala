package com.wbillingsley.handy

import scala.collection.concurrent.TrieMap

/**
 * A concurrent mutable cache for LazyIds. 
 */
class LookUpCache {
  
  /*
   * A mutable map of LazyId[T, K] to Ref[T] for any T.
   * 
   * The store method maintains this constraint.
   */
  private val cache = TrieMap.empty[LazyId[_, _], Ref[_]]
  
  private def find[T, K](l:LazyId[T, K]):Option[Ref[T]] = {    
    val tup = cache.get(l)
    
    // Note that the store method maintained the constraint in T
    tup map { _.asInstanceOf[Ref[T]] }
  }
  
  private def store[T, K](l:LazyId[T, K], r:Ref[T]) = {    
    cache.put(l, r)
    r
  }  
  
  def lookUp[T, K, KK](rbi: RefById[T, K]):Ref[T] = {    
    lookUp(LazyId(rbi.id)(rbi.lookUpMethod))
  }
  
  def lookUp[T, K, KK](li: LazyId[T, K]):Ref[T] = {    
    find(li).getOrElse(store(li, li))
  }  
    
  def remember[T, KK](r:Ref[T])(implicit g: GetsId[T, KK], lu:LookUpOne[T, KK]) = {
    r.getId match {
      case Some(id) => store(LazyId(id)(lu), r)
      case None => r
    }
  }
  
  def apply[T, KK](r: Ref[T])(implicit g: GetsId[T, KK], lu:LookUpOne[T, KK]):Ref[T] = {
    r match {
      case rbi:RefById[T, _] => lookUp(rbi)
      case li:LazyId[T, _] => lookUp(li)
      case r:Ref[T] => remember(r)
    }
  }  

  def apply[T, TT >: T, K, KK](rbi: RefById[T, K])(implicit g: GetsId[T, KK]):Ref[T] = {
    lookUp(rbi)
  }

  def apply[T, TT >: T, K, KK](li: LazyId[T, K])(implicit g: GetsId[T, KK]):Ref[T] = {
    lookUp(li)
  }
  
  
}