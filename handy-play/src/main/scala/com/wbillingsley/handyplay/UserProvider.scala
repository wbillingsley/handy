package com.wbillingsley.handyplay

import com.wbillingsley.handy.{Ref, RefOpt, RefNone}
import play.api.mvc.RequestHeader

trait UserProvider[U] extends UserFromRequest[U] {
  
  override def user(request:RequestHeader):RefOpt[U] = {
    request.session.get("sessionKey") match {
      case Some(sk) => bySessionKey(sk)
      case None => RefNone
    }    
  }
  
  def bySessionKey(sessionKey:String):RefOpt[U]
  
  def byIdentity(service:String, id:String):RefOpt[U]
  
  def byUsernameAndPassword(username:String, password:String):RefOpt[U]

  def byEmailAndPassword(email:String, password:String):RefOpt[U]

}

