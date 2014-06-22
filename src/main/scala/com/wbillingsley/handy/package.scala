package com.wbillingsley

/**
 * Created by wbillingsley on 1/02/2014.
 */
package object handy {

  type RefWithId[+T] = Ref[T] with IdImmediate[T]

  type LookUpOne[T, K] = Id[T,K] => Ref[T]

  type LookUpMany[T, K] = Ids[T,K] => RefMany[T]

}
