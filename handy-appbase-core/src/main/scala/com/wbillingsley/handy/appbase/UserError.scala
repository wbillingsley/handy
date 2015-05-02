package com.wbillingsley.handy.appbase

/** An error by the user. */
case class UserError(msg:String) extends Exception(msg)
