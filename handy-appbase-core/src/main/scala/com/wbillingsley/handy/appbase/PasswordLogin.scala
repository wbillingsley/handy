package com.wbillingsley.handy.appbase;

trait PasswordLoginT {


  val pwhash: Option[String]

  val username: Option[String]

  val email: Option[String]

}

case class PasswordLogin (

 pwhash: Option[String] = None,

 username: Option[String] = None,

 email:Option[String] = None

) extends PasswordLoginT
