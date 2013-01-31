package com.wbillingsley.handy.realistic

import com.wbillingsley.handy._


/*
 * Our implicit conversions for turning Ref into a response
 */

object ImplicitResponses {
  
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  
  implicit def refRespToFutResp(rp: Ref[Response]) = {
	val p = promise[Response]
    
    rp onComplete (
      onSuccess = p success _,
      onNone = p success PlainResponse(404, "Not found"),
      onFail = _ match {
        case Refused(msg) => p success PlainResponse(403, msg)
        case t:Throwable => p success PlainResponse(500, t.getMessage)
      }       
    )
    
    p.future
  }

  
}