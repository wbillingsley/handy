package com.wbillingsley.handyplay

import com.wbillingsley.handy._
import play.api.libs.json._

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
  
  /**
   * Converts a single item to JSON
   */
  implicit class ToJson[T, U](val item:T)(implicit jc:JsonConverter[T, U]) {
    
    def toJsonFor(approval:Approval[U]) = jc.toJsonFor(item, approval) 
    
    def toJson = jc.toJson(item)
  }    


}
