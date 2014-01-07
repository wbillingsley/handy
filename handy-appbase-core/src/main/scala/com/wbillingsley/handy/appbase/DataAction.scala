package com.wbillingsley.handy.appbase

import play.api.mvc.{EssentialAction, Request, RequestHeader, AnyContent, AcceptExtractors, BodyParser, BodyParsers, Action, SimpleResult, Results}

import com.wbillingsley.handy._
import Ref._
import com.wbillingsley.handyplay.RefConversions._
import JsonConverter._

import play.api.libs.iteratee.{Iteratee, Enumerator, Input, Step, Done}
import scala.concurrent.{Future, Promise}      
import play.api.libs.json.{ JsValue, Json }
import play.api.libs.concurrent.Execution.Implicits._

import scala.language.implicitConversions

/**
 * A bundle of extra information that can be returned from a data action to set headers, cookies, and Play session values
 */
case class HeaderInfo(
  /**
   * Adds headers to the result
   */
  headers: Seq[(String, String)] = Seq.empty,
  
  /**
   * Sets cookies
   */
  setCookies: Seq[play.api.mvc.Cookie] = Seq.empty,
  
  /**
   * Replaces the Play session. Note that DataAction will modify this to ensure the session key is set.
   */
  playSession: Option[play.api.mvc.Session] = None
)

/**
 * A single item (a result or something to turn to JSON) with info that should be added
 * to the headers for the response (eg cookies)
 */
class WithHeaderInfo[+T](val data: T, val headerInfo: Ref[HeaderInfo] = RefNone)

object WithHeaderInfo {
  /**
   * Allows DataActions just to return a Ref[T] as the promotion to OneWithHeaderInfo is inferred
   */
  implicit def apply[T](data:T, headerInfo:Ref[HeaderInfo] = RefNone) = new WithHeaderInfo(data, headerInfo)
  
  implicit def promote[T](data:T) = apply(data)
  
  /**
   * Allows DataAction.returning.one to return
   * myRef.withHeaderInfo(refHeaderInfo)
   */
  implicit class DecorateRef[T](val r:Ref[T]) extends AnyVal {
    def withHeaderInfo(rhi:Ref[HeaderInfo]) = WithHeaderInfo(r, rhi)
  }
  
  /**
   * Allows DataAction.returning.many actions to return
   * myRef.withHeaderInfo(refHeaderInfo)
   */
  class DecorateRefMany[T](val r:RefMany[T]) extends AnyVal {
    def withHeaderInfo(rhi:Ref[HeaderInfo]) = WithHeaderInfo(r, rhi)
  }
}

case class DataAction[U](implicit ufr:UserFromRequest[U]) {

  import DataAction._
  
  /*
   * Handles error conditions from a DataAction
   */
  private def handleErrors(r:Ref[Iteratee[Array[Byte], SimpleResult]])(implicit request:RequestHeader):Ref[Iteratee[Array[Byte], SimpleResult]] = { 
    // Convert failures into appropriate responses
    val recovered = r recoverWith {
      case Refused(msg) => request match {
        case Accepts.Html() => onForbidden(Refused(msg))(request).itself
        case Accepts.Json() => doneIteratee(Results.Forbidden(Json.obj("error" -> msg))).itself
        case _ => doneIteratee(Results.Forbidden(msg)).itself
      }
      case exc:Throwable if errorCodeMap.contains(exc.getClass()) => request match {
        case Accepts.Json() => doneIteratee(Results.Status(errorCodeMap(exc.getClass))(Json.obj("error" -> exc.getMessage()))).itself
        case _ => doneIteratee(Results.Status(errorCodeMap(exc.getClass))("User error in non-JSON request: " + exc.getMessage())).itself
      }
      case exc:Throwable => request match {
        case Accepts.Html() => onInternalServerError(exc)(request).itself
        case Accepts.Json() => doneIteratee(Results.InternalServerError(Json.obj("error" -> exc.getMessage))).itself
        case _ => doneIteratee(Results.InternalServerError(exc.getMessage)).itself
      }
    }
    
    // And handle the case where the response is that there is nothing
    val noneHandled = recovered orIfNone {
      request match {
        case Accepts.Html() => onNotFound(request).itself
        case Accepts.Json() => doneIteratee(Results.NotFound(Json.obj("error" -> "not found"))).itself
        case _ => doneIteratee(Results.NotFound).itself
      }
    }
    
    noneHandled
  }
  
  /*
   * Processes a Ref[SimpleResult] with headers into an Iteratee that can be returned from an action's apply method 
   */
  private def refSimpleResultToIteratee(request: AppbaseRequest[_, _], whi:WithHeaderInfo[Ref[SimpleResult]]) = {
    val refOriginal = for { simpleResult <- whi.data } yield doneIteratee(simpleResult)
    
    val refModified = for {
      // Get the header info; default it if there's none but fail if there's an error
      headerInf <- whi.headerInfo orIfNone HeaderInfo().itself
      
      // If the data has failed, produce a suitable response
      handled <- handleErrors(refOriginal)(request)
      
      // Add the header information to the response
      withHeaders = handled.map { sr => addHeaderInfoToResult(request, sr, headerInf) }
    } yield withHeaders
    
    // Convert the Ref[Iteratee[...]] to a Future[Iteratee[...]]
    val futModified = refModified.toFuture.map(_.getOrElse(doneIteratee(
      Results.InternalServerError("Error inside DataAction library: We handled the 'none' case but still there was nothing")
    )))
    
    // Flatten and return
    Iteratee.flatten(futModified)
  }
  
  /*
   * Processes a RefMany[Json] with headers into an Iteratee that can be returned from an action's apply method 
   */
  private def refManyJsonToIteratee(request: AppbaseRequest[_, _], whi:WithHeaderInfo[RefMany[JsValue]]) = {
    val res = whi.data whenReady { j =>
      Results.Ok.chunked(
        Enumerator("[") andThen j.enumerate.stringify andThen Enumerator("]") andThen Enumerator.eof[String]
      ).as("application/json")
    }
    refSimpleResultToIteratee(request, WithHeaderInfo(res, whi.headerInfo))
  }
  
  /**
   * An action returning a Result
   */
  def result(block: AppbaseRequest[AnyContent, U] => WithHeaderInfo[Ref[SimpleResult]]) = BodyAction(BodyParsers.parse.anyContent) { implicit request =>
    val wrapped = new AppbaseRequest(request)
    refSimpleResultToIteratee(wrapped, block(wrapped))
  }  

  
  /**
   * An action returning a Result
   */
  def result(block: => WithHeaderInfo[Ref[SimpleResult]]) = BodyAction(BodyParsers.parse.anyContent) { implicit request => 
    val wrapped = new AppbaseRequest(request)
    refSimpleResultToIteratee(wrapped, block)
  }
  
  /**
   * An action returning a Result
   */
  def result[A](bodyParser: BodyParser[A])(block: AppbaseRequest[A, U] => WithHeaderInfo[Ref[SimpleResult]]) = BodyAction(bodyParser) { implicit request => 
    val wrapped = new AppbaseRequest(request)
    refSimpleResultToIteratee(wrapped, block(wrapped))
  }
  
  /**
   * An action returning a Result
   */
  def json(block: AppbaseRequest[AnyContent, U] => WithHeaderInfo[Ref[JsValue]]) = BodyAction(BodyParsers.parse.anyContent) { implicit request => 
    request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        val wrapped = new AppbaseRequest(request)
        val whi = block(wrapped)
        val res = for (j <- whi.data) yield Results.Ok(j)
        refSimpleResultToIteratee(wrapped, WithHeaderInfo(res, whi.headerInfo))
      }
    }
  }
    
  /**
   * An action returning a Result
   */
  def json(block: => WithHeaderInfo[Ref[JsValue]]) = BodyAction(BodyParsers.parse.anyContent) { implicit request => 
    request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        val wrapped = new AppbaseRequest(request)
        val whi = block
        val res = for (j <- whi.data) yield Results.Ok(j)
        refSimpleResultToIteratee(wrapped, WithHeaderInfo(res, whi.headerInfo))
      }
    }
  }
  
  /**
   * An action returning a Result
   */
  def json[A](bodyParser: BodyParser[A])(block: AppbaseRequest[A, U] => WithHeaderInfo[Ref[JsValue]]) = BodyAction(bodyParser) { implicit request =>
    request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        val wrapped = new AppbaseRequest(request)
        val whi = block(wrapped)
        val res = for (j <- whi.data) yield Results.Ok(j)
        refSimpleResultToIteratee(wrapped, WithHeaderInfo(res, whi.headerInfo))
      }
    }
  }    
  
  
  /**
   * An action returning a single item that can be converted into JSON for the requesting user
   */
  def one[T](block: => WithHeaderInfo[Ref[T]])(implicit jc:JsonConverter[T, U]) = BodyAction(BodyParsers.parse.anyContent) { implicit request =>
    request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        val wrapped = new AppbaseRequest(request)
        val whi = block
        val res = for {
          item <- whi.data
          j <- jc.toJsonFor(item, wrapped.approval)
        } yield Results.Ok(j)
        refSimpleResultToIteratee(wrapped, WithHeaderInfo(res, whi.headerInfo))
      }
    }    
  }  
  

  /**
   * An action returning a single item that can be converted into JSON for the requesting user
   */
  def one[T](block: AppbaseRequest[AnyContent, U] => WithHeaderInfo[Ref[T]])(implicit jc:JsonConverter[T, U]) = BodyAction(BodyParsers.parse.anyContent) { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        val wrapped = new AppbaseRequest(request)
        val whi = block(wrapped)
        val res = for {
          item <- whi.data
          j <- jc.toJsonFor(item, wrapped.approval)
        } yield Results.Ok(j)
        refSimpleResultToIteratee(wrapped, WithHeaderInfo(res, whi.headerInfo))
      }
    }     
  }


  /**
   * An action returning a single item that can be converted into JSON for the requesting user
   */
  def one[T, A](bodyParser: BodyParser[A])(block: AppbaseRequest[A, U] => WithHeaderInfo[Ref[T]])(implicit jc:JsonConverter[T, U]) = BodyAction(bodyParser) { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        val wrapped = new AppbaseRequest(request)
        val whi = block(wrapped)
        val res = for {
          item <- whi.data
          j <- jc.toJsonFor(item, wrapped.approval)
        } yield Results.Ok(j)
        refSimpleResultToIteratee(wrapped, WithHeaderInfo(res, whi.headerInfo))
      }
    }         
  }
  
  
  /**
   * An action returning a many items that can be converted into JSON for the requesting user
   */
  def many[T](block: => WithHeaderInfo[RefMany[T]])(implicit jc:JsonConverter[T, U]) = BodyAction(BodyParsers.parse.anyContent) { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        val wrapped = new AppbaseRequest(request)
        val whi = block
        val json = whi.data.flatMap(jc.toJsonFor(_, wrapped.approval))
        refManyJsonToIteratee(wrapped, WithHeaderInfo(json, whi.headerInfo))
      }
    }    
  }  
  
  /**
   * An action returning a many items that can be converted into JSON for the requesting user
   */
  def many[T](block: AppbaseRequest[AnyContent, U] => WithHeaderInfo[RefMany[T]])(implicit jc:JsonConverter[T, U]) =  BodyAction(BodyParsers.parse.anyContent) { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        val wrapped = new AppbaseRequest(request)
        val whi = block(wrapped)
        val json = whi.data.flatMap(jc.toJsonFor(_, wrapped.approval))
        refManyJsonToIteratee(wrapped, WithHeaderInfo(json, whi.headerInfo))
      }
    }     
  }  
  
  /**
   * An action returning a many items that can be converted into JSON for the requesting user
   */
  def many[T, A](bodyParser: BodyParser[A])(block: AppbaseRequest[A, U] => WithHeaderInfo[RefMany[T]])(implicit jc:JsonConverter[T, U]) = BodyAction(bodyParser) { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        val wrapped = new AppbaseRequest(request)
        val whi = block(wrapped)
        val json = whi.data.flatMap(jc.toJsonFor(_, wrapped.approval))
        refManyJsonToIteratee(wrapped, WithHeaderInfo(json, whi.headerInfo))
      }
    }         
  }
  
  /**
   * An action returning a many items that can be converted into JSON for the requesting user
   */
  def manyJson(block: => WithHeaderInfo[RefMany[JsValue]]) = BodyAction(BodyParsers.parse.anyContent) { implicit request =>
    request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        val wrapped = new AppbaseRequest(request)
        val whi = block
        refManyJsonToIteratee(wrapped, whi)
      }
    }    
  }  
  
  /**
   * An action returning a many items that can be converted into JSON for the requesting user
   */
  def manyJson(block: AppbaseRequest[AnyContent, U] => WithHeaderInfo[RefMany[JsValue]]) = BodyAction(BodyParsers.parse.anyContent) { implicit request =>
    request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        val wrapped = new AppbaseRequest(request)
        val whi = block(wrapped)
        refManyJsonToIteratee(wrapped, whi)
      }
    }
  }  
  
  /**
   * An action returning a many items that can be converted into JSON for the requesting user
   */
  def manyJson[A](bodyParser: BodyParser[A])(block: AppbaseRequest[A, U] => WithHeaderInfo[RefMany[JsValue]]) = BodyAction(bodyParser) { implicit request =>
    request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        val wrapped = new AppbaseRequest(request)
        val whi = block(wrapped)
        refManyJsonToIteratee(wrapped, whi)
      }
    }
  }   
}


object DataAction extends AcceptExtractors {
  
  // Allows syntactic sugar of "DataAction returning one"
  def returning[U](implicit ufr:UserFromRequest[U]) = DataAction()(ufr)
  
  def doneIteratee[A](res:SimpleResult) = Done[Array[Byte], SimpleResult](res, Input.Empty)     
    
  
  /**
   * An EssentialAction with a body parser.
   * 
   * Solves an annoying issue with EssentialAction and Action not playing nicely together
   * (Action's apply method returns a very different type than EssentialAction's apply method. This is a pain if you want to 
   * write an Action -- ie, with a body parser -- that in its apply method can instead decide to call an EssentialAction)
   * 
   * So, this class provides us with an EssentialAction that takes a body parser (so we don't need to use Action to specify a BodyParser)
   */
  class BodyAction[A](parser:BodyParser[A])(block: Request[A] => Iteratee[Array[Byte], SimpleResult]) extends EssentialAction {
    def apply(rh: RequestHeader): Iteratee[Array[Byte], SimpleResult] = {
      parser(rh) flatMap { parseResult =>
        parseResult match {
          case Left(r) => doneIteratee(r)
          case Right(a) => {
            val request = Request(rh, a)
            block(request)
          }
        }
      }
    }
  }
  
  object BodyAction {
    def apply[A](parser:BodyParser[A])(block: Request[A] => Iteratee[Array[Byte], SimpleResult]) = new BodyAction(parser)(block)
  }
  
  var homeAction:EssentialAction = Action(Results.NotFound("No home action has been set"))
  
  var onNotFound:EssentialAction = Action(Results.NotFound("No 404 action has been set"))
  
  var onForbidden: Refused => EssentialAction = { (refused) => Action(Results.Forbidden("No forbidden action has been set")) }
  
  var onInternalServerError: Throwable => EssentialAction = { (exc) => Action(Results.InternalServerError("No 500 action has been set")) }
    
  
  /**
   * Ensures that the session key is set after this response. 
   * This might however prevent other session variables set in the contained action from sticking
   */
  def forceSession[A](a:Action[A])(implicit ufr:UserFromRequest[_]) = Action.async(a.parser) { implicit request =>
    val wrapped = new AppbaseRequest(request)
    val fresult = a(request)
    for (r <- fresult) yield addHeaderInfoToResult(wrapped, r)
  }
  
  /**
   * Applies the headerInfo to the result
   */
  protected def addHeaderInfoToResult(wrapped:AppbaseRequest[_, _], result:SimpleResult, headerInfo:HeaderInfo = HeaderInfo()) = {
    var r = result
    
    if (!headerInfo.headers.isEmpty) {
      r = r.withHeaders(headerInfo.headers:_*)
    }
    
    if (!headerInfo.setCookies.isEmpty) {
      r = r.withCookies(headerInfo.setCookies:_*)
    }
    
    headerInfo.playSession match {
      case Some(sess) => {
        r.withSession(sess + ("sessionKey" -> wrapped.sessionKey))
      }
      case None => {
        if (wrapped.session.get("sessionKey") == Some(wrapped.sessionKey)) {
          r
        } else {
          r.withSession(wrapped.session + ("sessionKey" -> wrapped.sessionKey))
        }
      }
    }
  }
  
  /**
   * Maps exceptions to HTTP error codes. For instance, you may wish to register your UserError exceptions as BadRequest
   */
  val errorCodeMap = scala.collection.mutable.Map.empty[Class[_], Int]
  
}