package com.wbillingsley.handy.appbase

import play.api.mvc.{Request, RequestHeader, AnyContent, AcceptExtractors, BodyParser, BodyParsers, Action, SimpleResult, Result, Results}
import play.api.libs.json.Json
import play.api.libs.iteratee.Enumerator
import com.wbillingsley.handy._
import com.wbillingsley.handyplay.RefConversions._
import JsonConverter._
import scala.language.implicitConversions
import play.api.mvc.EssentialAction
import play.api.libs.iteratee.{Iteratee, Input, Step, Done}    
import scala.concurrent.{Future, Promise}      


case class DataAction[U](implicit ufr:UserFromRequest[U]) extends AcceptExtractors {

  import DataAction._
  
  /**
   * An action returning a Result
   */
  def result(block: AppbaseRequest[AnyContent, U] => Ref[Result]) = EssentialAction { implicit request =>
    try {
      val ba = new BodyAction(BodyParsers.parse.anyContent)({ implicit request => 
        val wrapped = new AppbaseRequest(request)
        val res = for (item <- block(wrapped)) yield ensuringSessionKey(wrapped, item)
        val it = for (r <- res) yield doneIteratee(r)
        refEE(it)
      })
      ba.apply(request)
    } catch {
      case exc:Throwable => refEE(RefFailed(exc))
    }
  }
    
  /**
   * An action returning a Result
   */
  def result(block: => Ref[Result]) = EssentialAction { implicit request =>
    try {
      val ba = new BodyAction(BodyParsers.parse.anyContent)({ implicit request => 
        val wrapped = new AppbaseRequest(request)
        val res = for (item <- block) yield ensuringSessionKey(wrapped, item)
        val it = for (r <- res) yield doneIteratee(r)
        refEE(it)
      })
      val res = ba.apply(request)
      res
    } catch {
      case exc:Throwable => refEE(RefFailed(exc))
    }
  }
  
  /**
   * An action returning a Result
   */
  def result[A](bodyParser: BodyParser[A])(block: AppbaseRequest[A, U] => Ref[Result]) = EssentialAction { implicit request =>
    try {
      val ba = new BodyAction(bodyParser)({ implicit request => 
        val wrapped = new AppbaseRequest(request)
        val res = for (item <- block(wrapped)) yield ensuringSessionKey(wrapped, item)
        val it = for (r <- res) yield doneIteratee(r)
        refEE(it)
      })
      ba.apply(request)
    } catch {
      case exc:Throwable => refEE(RefFailed(exc))
    }
  }  
  
  
  /**
   * An action returning a single item that can be converted into JSON for the requesting user
   */
  def one[T](block: => Ref[T])(implicit jc:JsonConverter[T, U]) = EssentialAction { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        try {
          val ba = new BodyAction(BodyParsers.parse.anyContent)({ implicit request => 
            val wrapped = new AppbaseRequest(request)
            val res = for (item <- block; j <- jc.toJsonFor(item, wrapped.approval)) yield ensuringSessionKey(wrapped, Results.Ok(j))
            val it = for (r <- res) yield doneIteratee(r)
            refEE(it)
          })
          ba.apply(request)
        } catch {
          case exc:Throwable => refEE(RefFailed(exc))
        }
      }
    }    
  }  
  

  /**
   * An action returning a single item that can be converted into JSON for the requesting user
   */
  def one[T](block: AppbaseRequest[AnyContent, U] => Ref[T])(implicit jc:JsonConverter[T, U]) = EssentialAction { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        try {
          val ba = new BodyAction(BodyParsers.parse.anyContent)({ implicit request => 
            val wrapped = new AppbaseRequest(request)
            val res = for (item <- block(wrapped); j <- jc.toJsonFor(item, wrapped.approval)) yield ensuringSessionKey(wrapped, Results.Ok(j))
            val it = for (r <- res) yield doneIteratee(r)
            refEE(it)
          })
          ba.apply(request)          
        } catch {
          case exc:Throwable => refEE(RefFailed(exc))
        }
      }
    }     
  }


  /**
   * An action returning a single item that can be converted into JSON for the requesting user
   */
  def one[T, A](bodyParser: BodyParser[A])(block: AppbaseRequest[A, U] => Ref[T])(implicit jc:JsonConverter[T, U]) = EssentialAction { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        try {    
          val ba = new BodyAction(bodyParser)({ implicit request => 
            val wrapped = new AppbaseRequest(request)
            val res = for (item <- block(wrapped); j <- jc.toJsonFor(item, wrapped.approval)) yield ensuringSessionKey(wrapped, Results.Ok(j))
            val it = for (r <- res) yield doneIteratee(r)
            refEE(it)
          })
          ba.apply(request)           
        } catch {
          case exc:Throwable => refEE(RefFailed(exc))
        }
      }
    }         
  }
  
  
  /**
   * An action returning a many items that can be converted into JSON for the requesting user
   */
  def many[T](block: => RefMany[T])(implicit jc:JsonConverter[T, U]) = EssentialAction { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        try {
          val ba = new BodyAction(BodyParsers.parse.anyContent)({ implicit request => 
            val wrapped = new AppbaseRequest(request)
            val j = for (item <- block; j <- jc.toJsonFor(item, wrapped.approval)) yield j          
            val en = Enumerator("[") andThen j.enumerate.stringify andThen Enumerator("]") andThen Enumerator.eof[String]
            val r = ensuringSessionKey(wrapped, Results.Ok.stream(en).as("application/json"))
            doneIteratee(r)
          })
          ba.apply(request) 
        } catch {
          case exc:Throwable => refEE(RefFailed(exc))
        }
      }
    }    
  }  
  
  /**
   * An action returning a many items that can be converted into JSON for the requesting user
   */
  def many[T](block: AppbaseRequest[AnyContent, U] => RefMany[T])(implicit jc:JsonConverter[T, U]) = EssentialAction { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        try {
          val ba = new BodyAction(BodyParsers.parse.anyContent)({ implicit request => 
            val wrapped = new AppbaseRequest(request)
            val j = for (item <- block(wrapped); j <- jc.toJsonFor(item, wrapped.approval)) yield j          
            val en = Enumerator("[") andThen j.enumerate.stringify andThen Enumerator("]") andThen Enumerator.eof[String]
            val r = ensuringSessionKey(wrapped, Results.Ok.stream(en).as("application/json"))
            doneIteratee(r)
          })
          ba.apply(request) 
        } catch {
          case exc:Throwable => refEE(RefFailed(exc))
        }        
      }
    }     
  }  
  
  /**
   * An action returning a many items that can be converted into JSON for the requesting user
   */
  def many[T, A](bodyParser: BodyParser[A])(block: AppbaseRequest[A, U] => RefMany[T])(implicit jc:JsonConverter[T, U]) = EssentialAction { implicit request =>
	request match {
      case Accepts.Html() => homeAction(request)
      case Accepts.Json() => {
        try {
          val action = Action(bodyParser) { implicit request => 
            val wrapped = new AppbaseRequest(request)
            val j = for (item <- block(wrapped); j <- jc.toJsonFor(item, wrapped.approval)) yield j          
            val en = Enumerator("[") andThen j.enumerate.stringify andThen Enumerator("]") andThen Enumerator.eof[String]
            ensuringSessionKey(wrapped, Results.Ok.stream(en).as("application/json"))
          }
          action(request)
        } catch {
          case exc:Throwable => refEE(RefFailed(exc))
        }
      }
    }         
  }  
}


object DataAction extends AcceptExtractors {
  
  // Allows syntactic sugar of "DataAction returning one"
  def returning[U](implicit ufr:UserFromRequest[U]) = DataAction()(ufr)
  
  def doneIteratee[A](res:Result) = Done[Array[Byte], Result](res, Input.Empty)     
    
  
  /**
   * Solves an annoying issue with EssentialAction and Action not playing nicely together
   * (You can't get back from a Request[B] to a Request[AnyContent])
   */
  class BodyAction[A](parser:BodyParser[A])(block: Request[A] => Iteratee[Array[Byte], Result]) {
    
    
	def apply(rh: RequestHeader): Iteratee[Array[Byte], Result] = {
      
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
  
  var homeAction:EssentialAction = Action(Results.NotFound("No home action has been set"))
  
  var onNotFound:EssentialAction = Action(Results.NotFound("No 404 action has been set"))
  
  var onForbidden: Refused => EssentialAction = { (refused) => Action(Results.Forbidden("No forbidden action has been set")) }
  
  var onInternalServerError: Throwable => EssentialAction = { (exc) => Action(Results.InternalServerError("No 500 action has been set")) }
    
  
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
      result.withSession(wrapped.session + ("sessionKey" -> wrapped.sessionKey))
    }
  }
  
  
  /**
   * Maps exceptions to HTTP error codes. For instance, you may wish to register your UserError exceptions as BadRequest
   */
  val errorCodeMap = scala.collection.mutable.Map.empty[Class[_], Int]
  
  /**
   * Converts a Ref[Result] to an asychronous result
   */
  implicit def refEE(r:Ref[Iteratee[Array[Byte], Result]])(implicit request:RequestHeader):Iteratee[Array[Byte], Result] = { 
      import scala.concurrent._
      
      val p = promise[Iteratee[Array[Byte], Result]]
      r onComplete(
        onSuccess = p success _,
        onNone = p success {
          request match {
            case Accepts.Html() => onNotFound(request)
            case Accepts.Json() => Action(Results.NotFound(Json.obj("error" -> "not found")))(request)
            case _ => Action(Results.NotFound)(request)
          }
        },
        onFail = _ match {
          case Refused(msg) => p success {
            request match {
              case Accepts.Html() => onForbidden(Refused(msg))(request)
              case Accepts.Json() => Action(Results.Forbidden(Json.obj("error" -> msg)))(request)
              case _ => Action(Results.Forbidden(msg))(request)
            }            
          }
          case exc:Throwable if errorCodeMap.contains(exc.getClass()) => p success {
            request match {
              case Accepts.Json() => Action(Results.Status(errorCodeMap(exc.getClass))(Json.obj("error" -> exc.getMessage())))(request)
              case _ => Action(Results.Status(errorCodeMap(exc.getClass))("User error in non-JSON request: " + exc.getMessage()))(request)
            }            
          }
          case exc:Throwable => p success {
            
            exc.printStackTrace()
            
            request match {
              case Accepts.Html() => onInternalServerError(exc)(request)
              case Accepts.Json() => Action(Results.InternalServerError(Json.obj("error" -> exc.getMessage)))(request)
              case _ => Action(Results.InternalServerError(exc.getMessage))(request)
            }                        
          }
        }
      )
      Iteratee.flatten(p.future)
    
  }
  
}