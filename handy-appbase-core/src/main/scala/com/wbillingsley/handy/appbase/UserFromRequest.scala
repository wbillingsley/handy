package com.wbillingsley.handy.appbase

import play.api.mvc.Request
import com.wbillingsley.handy.Ref

/**
 * Provides the relevant class of user
 */
trait UserFromRequest[U] {
  
  def user(request:Request[_]):Ref[U]

}