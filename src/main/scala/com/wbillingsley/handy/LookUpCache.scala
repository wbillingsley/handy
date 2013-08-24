package com.wbillingsley.handy

/**
 * A mutable cache for Refs
 */
class LookUpCache {
  
  val cache = scala.collection.mutable.Map.empty[Any, List[(Class[_], Ref[_])]]
  
  private def cacheLookup[T, K, KK](rbi: RefById[T, K])(implicit g: GetsId[T, KK]):Ref[T] = {
    
    rbi.getId match {
      case Some(id) => { 
        val list = cache.getOrElse(id, Nil)        
        val assignable = for (
          (clazz, ref) <- list.find {
          	case (clazz, ref) => {
          	  rbi.clazz isAssignableFrom clazz
          	}
          }
        ) yield ref
        
        assignable.getOrElse({
          val l = rbi.lookUp
          remember(rbi.clazz, l)
          l 
        }).asInstanceOf[Ref[T]]
      }
      case None => rbi
    }

  }
  
  private def remember[T, KK](clazz: Class[_], r:Ref[T])(implicit g: GetsId[T, KK]) = {
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
      case ri:RefItself[T] => remember(clazz, ri)
      case r:Ref[T] => r
    }
  }  

  def apply[T, TT >: T, K, KK](rbi: RefById[T, K])(implicit g: GetsId[T, KK]):Ref[T] = {
	cacheLookup(rbi)
  }
  
  def apply[T, KK](ri: RefItself[T])(implicit g: GetsId[T, KK]):Ref[T] = {
	remember(ri.item.getClass, ri)
  }
  
}