package com.wbillingsley.handy.appbase

import play.api.mvc.{Request, AnyContent, AcceptExtractors, BodyParser, Action, SimpleResult, Result, Results}
import play.api.libs.json.Json
import play.api.libs.iteratee.Enumerator
import com.wbillingsley.handy._
import com.wbillingsley.handyplay.RefConversions._
import JsonConverter._

import scala.language.implicitConversions

object DataAction extends AcceptExtractors {
  
  var homeAction:Action[AnyContent] = Action(Results.NotFound("No home action has been set"))
  
  var onNotFound: (Request[AnyContent]) => SimpleResult[_] = { (request) => Results.NotFound("No 404 action has been set")}
  
  var onForbidden: (Refused, Request[AnyContent]) => SimpleResult[_] = { (refused, request) => Results.Forbidden("No forbidden action has been set") }
  
  var onInternalServerError: (Throwable, Request[AnyContent]) => SimpleResult[_] = { (exc, request) =>  Results.InternalServerError("No 500 action has been set") }
    
  
  /**
   * Ensures that the session key is set after this response. 
   * This might however prevent other session variables set in the contained action from sticking
   */
  def forceSession[A](a:Action[A])(implicit ufr:UserFromRequest[_]) = Action(a.parser) { implicit request =>
    val wrapped = new AppbaseRequest(request)
    ensuringSessionKey(wrapped, a(request))
  }
  
  protected def ensuringSessionKey(wrapped:AppbaseRequest[_, _], result:Result) = {
    if (wrapped.session.get("sessionKey") == Some(wrapped.sessionKey)) {
      result
    } else {
      result.withSession(wrapped.session + "sessionKey" -> wrapped.sessionKey)
    }
  }
  
  /**
   * An action returning a single item that can be converted into JSON for the requesting user
   */
  def one[T, U](block: => Ref[T])(implicit jc:JsonConverter[T, U], ufr:UserFromRequest[U]) = Action { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        try {
          val wrapped = new AppbaseRequest(request)
          val res = for (item <- block; j <- jc.toJsonFor(item, wrapped.approval)) yield ensuringSessionKey(wrapped, Results.Ok(j))
          refResultToResult(res)
        } catch {
          case exc:Throwable => refResultToResult(RefFailed(exc))
        }
      }
    }    
  }

  /**
   * An action returning a single item that can be converted into JSON for the requesting user
   */
  def one[T, U](block: (AppbaseRequest[AnyContent, U]) => Ref[T])(implicit jc:JsonConverter[T, U], ufr:UserFromRequest[U]) = Action { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        try {
          val wrapped = new AppbaseRequest(request)
          val res = for (item <- block(wrapped); j <- jc.toJsonFor(item, wrapped.approval)) yield ensuringSessionKey(wrapped, Results.Ok(j))
          refResultToResult(res)
        } catch {
          case exc:Throwable => refResultToResult(RefFailed(exc))
        }
      }
    }     
  }


  /**
   * An action returning a single item that can be converted into JSON for the requesting user
   */
  def one[T, U, A](bodyParser: BodyParser[A])(block: (Request[AnyContent]) => Ref[T])(implicit jc:JsonConverter[T, U], ufr:UserFromRequest[U]) = Action { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        try {
          val wrapped = new AppbaseRequest(request)
          val res = for (item <- block(wrapped); j <- jc.toJsonFor(item, wrapped.approval)) yield ensuringSessionKey(wrapped, Results.Ok(j))
          refResultToResult(res)
        } catch {
          case exc:Throwable => refResultToResult(RefFailed(exc))
        }
      }
    }         
  }
  
  
  /**
   * An action returning a many items that can be converted into JSON for the requesting user
   */
  def many[T, U](block: => RefMany[T])(implicit jc:JsonConverter[T, U], ufr:UserFromRequest[U]) = Action { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        try {
          val wrapped = new AppbaseRequest(request)
          val j = for (item <- block; j <- jc.toJsonFor(item, wrapped.approval)) yield j          
          val en = Enumerator("[") andThen j.enumerate.stringify andThen Enumerator("]") andThen Enumerator.eof[String]
          ensuringSessionKey(wrapped, Results.Ok.stream(en).as("application/json"))                              
        } catch {
          case exc:Throwable => refResultToResult(RefFailed(exc))
        }
      }
    }    
  }  
  
  /**
   * An action returning a many items that can be converted into JSON for the requesting user
   */
  def many[T, U](block: (AppbaseRequest[AnyContent, U]) => RefMany[T])(implicit jc:JsonConverter[T, U], ufr:UserFromRequest[U]) = Action { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        try {
          val wrapped = new AppbaseRequest(request)
          val j = for (item <- block(wrapped); j <- jc.toJsonFor(item, wrapped.approval)) yield j          
          val en = Enumerator("[") andThen j.enumerate.stringify andThen Enumerator("]") andThen Enumerator.eof[String]
          ensuringSessionKey(wrapped, Results.Ok.stream(en).as("application/json"))                             
        } catch {
          case exc:Throwable => refResultToResult(RefFailed(exc))
        }
      }
    }     
  }  
  
  /**
   * An action returning a many items that can be converted into JSON for the requesting user
   */
  def many[T, U, A](bodyParser: BodyParser[A])(block: (Request[AnyContent]) => RefMany[T])(implicit jc:JsonConverter[T, U], ufr:UserFromRequest[U]) = Action { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        try {
          val wrapped = new AppbaseRequest(request)
          val j = for (item <- block(wrapped); j <- jc.toJsonFor(item, wrapped.approval)) yield j          
          val en = Enumerator("[") andThen j.enumerate.stringify andThen Enumerator("]") andThen Enumerator.eof[String]
          ensuringSessionKey(wrapped, Results.Ok.stream(en).as("application/json"))                            
        } catch {
          case exc:Throwable => refResultToResult(RefFailed(exc))
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
  implicit def refResultToResult(r:Ref[Result])(implicit request:Request[AnyContent]) = {
    Results.Async {
      import scala.concurrent._
      
      val p = promise[Result]
      r onComplete(
        onSuccess = p success _,
        onNone = p success {
          request match {
            case Accepts.Html() => onNotFound(request)
            case Accepts.Json() => Results.NotFound(Json.obj("error" -> "not found"))
            case _ => Results.NotFound
          }
        },
        onFail = _ match {
          case Refused(msg) => p success {
            request match {
              case Accepts.Html() => onForbidden(Refused(msg), request)
              case Accepts.Json() => Results.Forbidden(Json.obj("error" -> msg))
              case _ => Results.Forbidden(msg)
            }            
          }
          case exc:Throwable if errorCodeMap.contains(exc.getClass()) => p success {
            request match {
              case Accepts.Json() => Results.Status(errorCodeMap(exc.getClass))(Json.obj("error" -> exc.getMessage()))
              case _ => Results.Status(errorCodeMap(exc.getClass))("User error in non-JSON request: " + exc.getMessage())
            }            
          }
          case exc:Throwable => p success {
            
            exc.printStackTrace()
            
            request match {
              case Accepts.Html() => onInternalServerError(exc, request)
              case Accepts.Json() => Results.InternalServerError(Json.obj("error" -> exc.getMessage))
              case _ => Results.InternalServerError(exc.getMessage)
            }                        
          }
        }
      )
      p.future
    }
  }  
  
}