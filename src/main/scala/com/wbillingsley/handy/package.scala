package com.wbillingsley

/**
 * Created by wbillingsley on 1/02/2014.
 */
package object handy {

  type RefWithId[+T] = Ref[T] with IdImmediate[T]

}
