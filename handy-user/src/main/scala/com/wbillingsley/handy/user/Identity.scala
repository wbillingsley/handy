package com.wbillingsley.handy.user

trait Identity {
  
    val service:String
    
    val value:String
    
    val avatar:Option[String] 
    
    val username:Option[String]
    
    val since:Long
    
}

object Identity {
    
    def defaultSince = System.currentTimeMillis

}
