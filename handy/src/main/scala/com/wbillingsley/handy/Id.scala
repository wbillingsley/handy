package com.wbillingsley.handy
import scala.concurrent.Future

/*
 * This file contains classes that are helpful with managing Ids
 */


/**
  * An ID of an item.
  * 
  * This is now defined as a trait instead of a case class. This is because if an Id is used as a key in a collection, we
  * can face the issue that, say, Id[Apple, Int](1) might be considered equal to Id[Orange, Int](1) anywhere that the
  * type has not been preserved (e.g. if you want to keep a single cache instead of one for each type).
  * 
  * Consequently, it can be useful to subclass Id:
  * case class AppleId(i:Int) extends Id[Apple, Int]
  * case class OrangeId(i:Int) extends Id[Orange, Int]
  * That also helps with runtime inspection of an object, as the information on what it's an Id for is preserved
  * (whereas otherwise type erasure would mean it isn't)
  *
  * Though there's nothing stopping you from:
  * case class AnyId[T](i:Int) extends Id[T, Int]
  * if you'd really like them all to be in one class.
  *
  * @param id
  * @tparam T covariant: the Id of a horse is an Id of a mammal
  * @tparam K invariant.
  */
trait Id[+T,K] {
  def id: K
}

extension [T, K, C <: Id[T, K]] (id:C) {
  def lazily(using lu:EagerLookUpOne[C, T]) = LazyId(id)(using lu)
  
  def lookUp(using lu:EagerLookUpOne[C, T]) = lu.apply(id)

  def lookUpOpt(using lu:EagerLookUpOpt[C, T]) = lu.apply(id)

  def lookUpTask(using lu:LazyLookUpOne[C, T]) = lu.apply(id)

  def lookUpOptTask(using lu:LazyLookUpOpt[C, T]) = lu.apply(id)
}

implicit object Id {
  
  extension [T, K, C <: Id[T, K]] (id:K)(using f: K => C) {
    def asId:C = f(id)
  }

  extension [T, K, C <: Ids[T, K]] (ids:Seq[Id[T, K]])(using f: Seq[K] => C) {
    def asIds:C = f(ids.map(_.id))
  }

}