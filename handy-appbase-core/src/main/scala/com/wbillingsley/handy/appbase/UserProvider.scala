package com.wbillingsley.handy.appbase

import com.wbillingsley.handy.{Ref, RefNone}
import play.api.mvc.Request

trait UserProvider[U] extends UserFromRequest[U] {
  
  final def user(request:Request[_]):Ref[U] = {    
    request.session.get("sessionKey") match {
      case Some(sk) => bySessionKey(sk)
      case None => RefNone
    }    
  }
  
  def bySessionKey(sessionKey:String):Ref[U]
  
  def byIdentity(service:String, id:String):Ref[U]
  
  def byUsernameAndPassword(username:String, password:String)

  def byEmailAndPassword(email:String, password:String)

}

