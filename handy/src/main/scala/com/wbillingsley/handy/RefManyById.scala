package com.wbillingsley.handy

import Ref._

/**
 * Refers to many items by their ID.
 */
case class RefManyById[K, T](ids: K)(using lu:EagerLookUpMany[K, T]) extends RefMany[T] {

  final lazy val lookedUp = lu(ids)
  
  export lookedUp.first
  export lookedUp.flatMapOne
  export lookedUp.flatMapOpt
  export lookedUp.flatMapMany
  export lookedUp.foldLeft
  export lookedUp.foreach
  export lookedUp.recoverManyWith
  export lookedUp.withFilter
  export lookedUp.map
  export lookedUp.whenReady

}

object RefManyById {

  /**
   * An empty RefMany that always resolves to nothing.
   *
   * Note that it takes two type parameters as RefManyById is invariant in K (so we can't just
   * return a RefManyById[T, Any]
   */
  def empty[K, T] = RefManyById(Seq.empty[T])(using (_) => RefEmpty)
  
}
