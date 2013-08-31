package com.wbillingsley.handy.appbase

import com.wbillingsley.encrypt.Encrypt
import com.wbillingsley.handy.Ref

trait User[I] {
  
  val id:String
    
  var username: Option[String]

  var salt: Option[String] = Some(Encrypt.genSaltB64)

  var pwhash: Option[String] = None

  var email: Option[String] = None

  var name: Option[String] = None

  var nickname: Option[String] = None

  var identities: Seq[I] = Seq.empty

  var avatar: Option[String] = None

  val created: Long = System.currentTimeMillis

  def hash(password: String) = for (s <- salt) yield Encrypt.encrypt(s, password)

}


trait UserDAO[U, I] {
  
  def bySessionKey(sessionKey:String):Ref[U]
  
  def byIdentity(service:String, id:String):Ref[U]
  
  def addSession(user:Ref[U], sessionKey:String):Ref[U]
  
  def removeSession(user:Ref[U], sessionKey:String):Ref[U]
  
  def addIdentity(service:String, id:String, identity:I):Ref[U]
  
  def removeIdentity(service:String, id:String, identity:I):Ref[U]

}