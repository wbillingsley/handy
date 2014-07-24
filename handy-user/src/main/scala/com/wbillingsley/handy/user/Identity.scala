package com.wbillingsley.handy.user

trait IdentityT {
  
    val service:String
    
    val value:String
    
    val avatar:Option[String] 
    
    val username:Option[String]
    
    val since:Long
    
}

object Identity {
    
    def defaultSince = System.currentTimeMillis

}

case class Identity (

  service: String,

  value: String,

  avatar: Option[String] = None,

  username: Option[String] = None,

  since: Long = Identity.defaultSince

) extends IdentityT