package com.wbillingsley.handy.appbase

import com.wbillingsley.handy._
import play.api.libs.json._
import com.wbillingsley.handy.Ref._
import play.api.libs.iteratee.Enumerator
import com.wbillingsley.handy.Approval.wrapApproval
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import scala.concurrent.ExecutionContext

trait JsonConverter[T, U] {
    
  def toJsonFor(item:T, approval:Approval[U]):Ref[JsValue]
 
  def toJson(item:T):Ref[JsValue]
  
}
  
object JsonConverter {
  
  /*
   * We're using this onthe grounds that it is variable, and that if you're in the Play environment you'll
   * probably be pointing it at the threadpool where you want your work to be done anyway
   */  
  import RefFuture.executionContext
  
  /**
   * Converts a single item to JSON
   */
  implicit class ToJson[T, U](val item:T)(implicit jc:JsonConverter[T, U]) {
    
    def toJsonFor(approval:Approval[U]) = jc.toJsonFor(item, approval) 
    
    def toJson = jc.toJson(item)
  }    
  
  /** 
   * Useful for putting the commas in the right place when Enumerating JSON as a string
   */
  implicit class StringifyJson(val en: Enumerator[JsObject]) extends AnyVal {
    def stringify(implicit ec:ExecutionContext) = {
      var sep = ""
      for (j <- en) yield {
        val s = sep + j.toString
        sep = ","
        s
      }
    }
  }  

  /** 
   * Useful for putting the commas in the right place when Enumerating JSON as a string.
   * We have both this and StringifyJson because Enumerator[T] is invariant on T
   */
  implicit class StringifyJsValue[J <: JsValue](val en: Enumerator[J]) extends AnyVal {
    def stringify(implicit ec:ExecutionContext) = {
      var sep = ""
      for (j <- en) yield {
        val s = sep + j.toString
        sep = ","
        s
      }
    }
  }    
    
}
