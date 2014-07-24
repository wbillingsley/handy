package com.wbillingsley.handyplay

import play.api.mvc.RequestHeader
import com.wbillingsley.handy.Ref

/**
 * Provides the relevant class of user
 */
trait UserFromRequest[U] {
  
  def user(request:RequestHeader):Ref[U]

}