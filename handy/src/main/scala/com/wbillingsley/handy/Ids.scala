package com.wbillingsley.handy

trait Ids[+T, K] {
  
  def ids: Seq[K]

  def contains[TT >: T](id:Id[TT, K]) = ids.contains(id.id)
  
  def toSeqId[C >: Id[T, K]](using f: K => C) = ids.map((x) => f(x))

}


implicit object Ids {

  extension [T, K, C <: Ids[T, K]] (ids:Seq[K])(using f: Seq[K] => C) {
    def asIds:C = f(ids)
  }

  extension [T, K, C <: Ids[T, K]] (ids:C) {
    def lookUp(using lu:EagerLookUpMany[C, T]) = lu.apply(ids)
  }
  
}

