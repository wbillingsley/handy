package com.wbillingsley.handy


/**
 * A concurrent mutable cache for Id and LookUps
 */
class LookUpCache {

  import java.util.concurrent

  case class LookUpPair[C,T](id:C, lu:LookUp[C,T])

  private val cache = new concurrent.ConcurrentHashMap[LookUpPair[_,_],Ref[_]]()

  // Note that we constrain the T -> Ref[T] relationship by what we put in the map
  private def get[C, T](pair:LookUpPair[C, T]):Option[Ref[T]] = Option(cache.get(pair).asInstanceOf[Ref[T]])

  // Note that we constrain the T -> Ref[T] relationship by what we put in the map
  private def storeAndLookUp[C, T](pair:LookUpPair[C, T]):Ref[T] = {
    val r:Ref[T] = pair.lu.eagerOne(pair.id)
    store(pair, r)
  }

  private def store[C, T](pair:LookUpPair[C, T], r:Ref[T]):Ref[T] = {
    cache.put(pair, r)
    r
  }

  /**
   * Looks up the Id in the cache, or looks it up and stores it in the case of a cache miss.
   */
  def lookUp[C, T](id:C)(implicit lu:LookUp[C, T]):Ref[T] = {
    val pair = LookUpPair(id, lu)
    get(pair) getOrElse storeAndLookUp(pair)
  }

  def replace[C,T](id:C)(using lu:LookUp[C,T]):Ref[T] = {
    val pair = LookUpPair(id, lu)
    storeAndLookUp(pair)
  }

  def replace[C, T](id:C, item:T)(implicit lu:LookUp[C, T]):Ref[T] = {
    val pair = LookUpPair(id, lu)
    store(pair, RefItself(item))
  }

  def remove[C, T](id:C)(implicit lu:LookUp[C, T]): Unit = {
    val pair = LookUpPair(id, lu)
    cache.remove(pair)
  }

  /**
   * Looks up the Id in the cache, or looks it up and stores it in the case of a cache miss.
   */
  def apply[C, T](id:C)(implicit lu:LookUp[C, T]):Ref[T] = lookUp(id)(using lu)
  
}
