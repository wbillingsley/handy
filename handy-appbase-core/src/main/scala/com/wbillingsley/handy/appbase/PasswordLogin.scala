package com.wbillingsley.handy.appbase

import com.wbillingsley.encrypt.Encrypt

trait PasswordLogin {
    
  def hash(password: String) = for (s <- salt) yield Encrypt.encrypt(s, password)  

  val salt: Option[String]

  val pwhash: Option[String]

  val username: Option[String]

  val email: Option[String]
  
}

object PasswordLogin {
  
  def defaultSalt = Some(Encrypt.genSaltB64)

}