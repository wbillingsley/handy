package com.wbillingsley.handy

import scala.concurrent.Future

/**
 * A Ref by id, that uses a lazy val to memoise the result of looking it up.
 */
case class LazyId[C, T](val id: C)(using lookUpMethod:EagerLookUpOne[C, T]) extends Ref[T] {

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
