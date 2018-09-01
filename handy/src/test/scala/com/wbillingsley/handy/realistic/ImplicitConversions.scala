package com.wbillingsley.handy.realistic

import com.wbillingsley.handy._
import scala.language.implicitConversions

/*
 * Our implicit conversions for turning Ref into a response
 */

object ImplicitResponses {
  
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._

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