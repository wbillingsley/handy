package com.wbillingsley.handy.appbase

import play.api.mvc.{WrappedRequest, Request}
import com.wbillingsley.handy.Approval

class AppbaseRequest[A, U](request:Request[A])(implicit ufr:UserFromRequest[U]) extends WrappedRequest(request) {

  val user = ufr.user(request)
  
  val approval = new Approval(user)
  
}