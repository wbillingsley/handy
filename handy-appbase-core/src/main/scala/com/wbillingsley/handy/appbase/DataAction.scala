package com.wbillingsley.handy.appbase

import play.api.mvc.{Request, RequestHeader, AnyContent, AcceptExtractors, BodyParser, BodyParsers, Action, SimpleResult, Results}
import play.api.libs.json.Json
import play.api.libs.iteratee.Enumerator
import com.wbillingsley.handy._
import Ref._
import com.wbillingsley.handyplay.RefConversions._
import JsonConverter._
import scala.language.implicitConversions
import play.api.mvc.EssentialAction
import play.api.libs.iteratee.{Iteratee, Input, Step, Done}
import scala.concurrent.{Future, Promise}      
import play.api.libs.json.JsValue


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
   * Processes a Ref[SimpleResult] with headers into an Iteratee that can be returned from an action's apply method 
   */
  private def refSimpleResultToIteratee(request: AppbaseRequest[_, _], whi:WithHeaderInfo[Ref[SimpleResult]]) = {
    val it = for {
      headerInf <- whi.headerInfo orIfNone HeaderInfo().itself
      result <- whi.data
      modified = addHeaderInfoToResult(request, result, headerInf)
    } yield doneIteratee(modified)
    refEE(it)(request)
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
    val wrapped = new AppbaseRequest(request)
    val whi = block(wrapped)
    val res = for (j <- whi.data) yield Results.Ok(j)
    refSimpleResultToIteratee(wrapped, WithHeaderInfo(res, whi.headerInfo))
  }
    
  /**
   * An action returning a Result
   */
  def json(block: => WithHeaderInfo[Ref[JsValue]]) = BodyAction(BodyParsers.parse.anyContent) { implicit request => 
    val wrapped = new AppbaseRequest(request)
    val whi = block
    val res = for (j <- whi.data) yield Results.Ok(j)
    refSimpleResultToIteratee(wrapped, WithHeaderInfo(res, whi.headerInfo))
  }
  
  /**
   * An action returning a Result
   */
  def json[A](bodyParser: BodyParser[A])(block: AppbaseRequest[A, U] => WithHeaderInfo[Ref[JsValue]]) = BodyAction(bodyParser) { implicit request =>
    val wrapped = new AppbaseRequest(request)
    val whi = block(wrapped)
    val res = for (j <- whi.data) yield Results.Ok(j)
    refSimpleResultToIteratee(wrapped, WithHeaderInfo(res, whi.headerInfo))
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
  
  import RefFuture.executionContext
  
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
  
  /**
   * Converts a Ref[Result] to an asychronous result
   */
  implicit def refEE(r:Ref[Iteratee[Array[Byte], SimpleResult]])(implicit request:RequestHeader):Iteratee[Array[Byte], SimpleResult] = { 
      import scala.concurrent._
      
      val p = promise[Iteratee[Array[Byte], SimpleResult]]
      r onComplete(
        onSuccess = p success _,
        onNone = p success {
          request match {
            case Accepts.Html() => onNotFound(request)
            case Accepts.Json() => doneIteratee(Results.NotFound(Json.obj("error" -> "not found")))
            case _ => doneIteratee(Results.NotFound)
          }
        },
        onFail = _ match {
          case Refused(msg) => p success {
            request match {
              case Accepts.Html() => onForbidden(Refused(msg))(request)
              case Accepts.Json() => doneIteratee(Results.Forbidden(Json.obj("error" -> msg)))
              case _ => doneIteratee(Results.Forbidden(msg))
            }            
          }
          case exc:Throwable if errorCodeMap.contains(exc.getClass()) => p success {
            request match {
              case Accepts.Json() => doneIteratee(Results.Status(errorCodeMap(exc.getClass))(Json.obj("error" -> exc.getMessage())))
              case _ => doneIteratee(Results.Status(errorCodeMap(exc.getClass))("User error in non-JSON request: " + exc.getMessage()))
            }            
          }
          case exc:Throwable => p success {
            request match {
              case Accepts.Html() => onInternalServerError(exc)(request)
              case Accepts.Json() => doneIteratee(Results.InternalServerError(Json.obj("error" -> exc.getMessage)))
              case _ => doneIteratee(Results.InternalServerError(exc.getMessage))
            }                        
          }
        }
      )
      Iteratee.flatten(p.future)
    
  }
  
}