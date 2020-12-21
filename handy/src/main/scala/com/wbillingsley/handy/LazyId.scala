package com.wbillingsley.handy

import scala.concurrent.Future

/**
 * A Ref by id, that uses a lazy val to memoise the result of looking it up.
 */
case class LazyId[C, T](val id: C)(using lookUpMethod:EagerLookUpOne[C, T]) extends Ref[T] {

  // The immediate id should just be id, but the type-checker can't know that the type Key is the type C, so 
  // we have to call "canonical"
  override def immediateId[TT >: T, Key <: Id[TT, _]](implicit g:GetsId[TT, Key]):Option[Key] = g.canonical(id)

  final lazy val lookUp = lookUpMethod(id)
  
  export lookUp.toFuture
  export lookUp.flatMapOne
  export lookUp.flatMapOpt
  export lookUp.flatMapMany
  export lookUp.map
  export lookUp.recoverWith
  export lookUp.foreach

}

object LazyId {


}
