package com.wbillingsley.handy.realistic

import com.wbillingsley.handy._
import scala.language.implicitConversions

/*
 * Our implicit conversions for turning Ref into a response
 */

object ImplicitResponses {
  
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  
  implicit def refRespToFutResp(rp: Ref[Response]) = {
	val p = Promise[Response]
    
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
  
  

  /**
   * This is left here to give a hint as to how this is done, but I've not imported
   * the ReactiveMongo classes into this project's test scope yet.
   */
  implicit class RefEnumerator[T](val refMany:RefMany[T]) {
    
    def enumerator = {
      refMany match {
        // case RefRMCursor(c) => c.enumerate
        case _ => {
          /*
           * play.api.libs.iteratee.Concurrent.unicast[T] { channel =>	    	    
	    	    for (r <- refMany) {
	              channel.push(r)
	            }
	    	  }
           */
        }
      }
    }
    
  } 

  
}