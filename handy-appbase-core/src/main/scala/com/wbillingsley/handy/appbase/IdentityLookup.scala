package com.wbillingsley.handy.appbase

case class IdentityLookup(service:String,
                          value:Option[String],
                          username:Option[String])
