package com.wbillingsley.handy

/**
 * A concurrent mutable cache for Id and LookUps
 */
class LookUpCache {

  import java.util.concurrent

  case class LookUpPair[T,K](id:Id[T,K], lu:LookUp[T,K])

  private val cache = new concurrent.ConcurrentHashMap[LookUpPair[_,_],Ref[_]]()

  // Note that we constrain the T -> Ref[T] relationship by what we put in the map
  private def get[T,K](pair:LookUpPair[T,K]):Option[Ref[T]] = Option(cache.get(pair).asInstanceOf[Ref[T]])

  // Note that we constrain the T -> Ref[T] relationship by what we put in the map
  private def storeAndLookUp[T,K](pair:LookUpPair[T,K]):Ref[T] = {
    val r:Ref[T] = pair.lu.one[K](pair.id)
    store(pair, r)
  }

  private def store[T,K](pair:LookUpPair[T,K], r:Ref[T]):Ref[T] = {
    cache.put(pair, r)
    r
  }

  /**
   * Looks up the Id in the cache, or looks it up and stores it in the case of a cache miss.
   */
  def lookUp[T,K](id:Id[T,K])(implicit lu:LookUp[T,K]):Ref[T] = {
    val pair = LookUpPair(id, lu)
    get(pair) getOrElse storeAndLookUp(pair)
  }

  def lookUpOrStore[T, KK](r: Ref[T])(implicit g: GetsId[T, KK], lu:LookUp[T, KK]):Ref[T] = {
    r match {
      case li:LazyId[T, KK] => {
        val rPair = for {
          id <- li.refId(g)
        } yield LookUpPair(id, lu)

        rPair flatMap { pair =>
          get(pair) getOrElse storeAndLookUp(pair)
        }
      }
      case r:Ref[T] => {
        r.refId flatMap { id =>
          val pair = LookUpPair(id, lu)
          store(pair, r)
        }
      }
    }
  }

  /**
   * Looks up the Id in the cache, or looks it up and stores it in the case of a cache miss.
   */
  def apply[T,K](id:Id[T,K])(implicit lu:LookUp[T,K]):Ref[T] = lookUp(id)(lu)

  def apply[T, K](r: Ref[T])(implicit g: GetsId[T, K], lu:LookUp[T, K]):Ref[T] = lookUpOrStore(r)(g, lu)
}
