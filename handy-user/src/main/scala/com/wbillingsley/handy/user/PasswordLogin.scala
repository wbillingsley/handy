package com.wbillingsley.handy.user

import org.mindrot.jbcrypt.BCrypt;

trait PasswordLoginT {
    
  def hash(password: String) = BCrypt.hashpw(password, BCrypt.gensalt())

  def checkPassword(pw:String) = pwhash match {
    case Some(hashed) => BCrypt.checkpw(pw, hashed)
    case None => false
  }

  val pwhash: Option[String]

  val username: Option[String]

  val email: Option[String]
  
}

case class PasswordLogin (

 pwhash: Option[String] = None,

 username: Option[String] = None,

 email:Option[String] = None

) extends PasswordLoginT
