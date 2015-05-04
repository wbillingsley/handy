package com.wbillingsley.handy.appbase

trait IdentityT {

    val service:String

    val value:Option[String]

    val avatar:Option[String]

    val username:Option[String]

    val since:Long

}

object Identity {

    def defaultSince = System.currentTimeMillis

}

case class Identity (

  service: String,

  value: Option[String],

  avatar: Option[String] = None,

  username: Option[String] = None,

  since: Long = Identity.defaultSince

) extends IdentityT {


  def matches(service:String, value:Option[String], username:Option[String]):Boolean = {
    this.service == service &&
      (value.nonEmpty && this.value == value) || (username.nonEmpty && this.username == username)
  }

}
