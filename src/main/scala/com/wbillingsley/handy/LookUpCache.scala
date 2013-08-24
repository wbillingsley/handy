package com.wbillingsley.handy

/**
 * A mutable cache for Refs
 */
class LookUpCache {
  
  case class Key[T, ID](clazz: Class[T], id:Option[ID])
  
  val cache = scala.collection.mutable.Map.empty[Key[_, _], Ref[_]]
  
  private def cacheLookup[T, ID](rbi: RefById[T, ID]):Ref[T] = {
    val key = Key(rbi.clazz, rbi.getId)
    cache.get(key).getOrElse({
      val l = rbi.lookUp
      cache.put(key, l)
      l
    }).asInstanceOf[Ref[T]]
  }
  
  private def remember[T, KK](ri:RefItself[T])(implicit g: GetsId[T, KK]) = {
    val key = Key(ri.item.getClass, ri.getId)
    cache.put(key, ri)
    ri
  }
  
  def apply[T, KK](r: Ref[T])(implicit g: GetsId[T, KK]):Ref[T] = {
    r match {
      case rbi:RefById[T, _] => cacheLookup(rbi)
      case ri:RefItself[T] => remember(ri)
      case r:Ref[T] => r
    }
  }  

}