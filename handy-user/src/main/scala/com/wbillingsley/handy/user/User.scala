package com.wbillingsley.handy.user

import com.wbillingsley.handy.{HasStringId, Id, Ref}

trait UserT[U, I <: IdentityT, PL <: PasswordLoginT] {
  
  val id:Id[U,String]
    
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

case class User(

  id:Id[User,String],

  name:Option[String] = None,

  nickname:Option[String] = None,

  avatar:Option[String] = None,

  pwlogin: PasswordLogin = PasswordLogin(),

  secret: String = scala.util.Random.alphanumeric.take(16).mkString,

  identities:Seq[Identity] = Seq.empty,

  activeSessions:Seq[ActiveSession] = Seq.empty,

  created: Long = User.defaultCreated

) extends UserT[User, Identity, PasswordLogin] with HasStringId[User] {

  def getIdentity(service:String) = identities.find(p => p.service == service)

}