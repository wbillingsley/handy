package com.wbillingsley.handyplay

import play.api.mvc.RequestHeader
import com.wbillingsley.handy.RefOpt

/**
 * Provides the relevant class of user
 */
trait UserFromRequest[U] {
  
  def user(request:RequestHeader):RefOpt[U]

}