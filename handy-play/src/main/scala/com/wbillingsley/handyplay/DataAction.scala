package com.wbillingsley.handyplay

import akka.util.ByteString
import play.api.Logger
import play.api.libs.streams.Accumulator
import play.api.mvc.{EssentialAction, Request, RequestHeader, AnyContent, AcceptExtractors, BodyParser, BodyParsers, Action, Result, Results}

import com.wbillingsley.handy._
import Ref._
import com.wbillingsley.handyplay.JsonConverter._
import com.wbillingsley.handyplay.RefConversions._
import play.api.libs.iteratee._
import scala.concurrent.{Future, Promise}
import play.api.libs.json.{ JsValue, Json }
import play.api.libs.concurrent.Execution.Implicits._

import scala.language.implicitConversions


/**
 * Configuration for a DataAction. This sets, for instance 
 */
trait DataActionConfig {

  /**
   * Whether or not to redirect HTML
   */
  def redirectHtmlRequests:Boolean
  
  /**
   * The action to perform when a request accepts HTML. This should be your application's home page.
   * TODO: Add a switch for whether or not to redirect HTML-accepting requests
   */
  def homeAction:EssentialAction

  /**
   * The action for HTML requests that are not found.
   */
  def onNotFound:EssentialAction = Action(Results.NotFound("Not found"))
  
  /**
   * The action for HTML requests that are forbidden.
   */
  def onForbidden: Refused => EssentialAction = { (refused) => Action(Results.Forbidden(refused.getMessage)) }
  
  /**
   * The action for HTML requests that fail.
   */
  def onInternalServerError: Throwable => EssentialAction = { (exc) => Action(Results.InternalServerError(exc.getMessage)) }

  /**
   * Maps failures to HTTP error codes. For instance, you may wish to register your UserError exceptions as BadRequest so
   * that they are not returned to the client as InternalServerError.
   */
  def errorCodeMap:Map[Class[_], Int]
}

