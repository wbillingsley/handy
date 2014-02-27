package com.wbillingsley.handy.user

import com.wbillingsley.encrypt.Encrypt
import com.wbillingsley.handy.Ref

trait User[I <: Identity, PL <: PasswordLogin] {
  
  val id:String
    
  val name: Option[String]

  val nickname: Option[String]
 
  val pwlogin: PL

  val identities: Seq[I]

  val avatar: Option[String] 

  val created: Long 

}

object User {
  
  def defaultCreated = System.currentTimeMillis  

}

trait UserDAO[U, I] {
  
  def bySessionKey(sessionKey:String):Ref[U]
  
  def byIdentity(service:String, id:String):Ref[U]
  
  def addSession(user:Ref[U], sessionKey:String):Ref[U]
  
  def removeSession(user:Ref[U], sessionKey:String):Ref[U]
  
  def addIdentity(service:String, id:String, identity:I):Ref[U]
  
  def removeIdentity(service:String, id:String, identity:I):Ref[U]

}