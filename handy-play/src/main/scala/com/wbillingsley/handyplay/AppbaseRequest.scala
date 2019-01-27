package com.wbillingsley.handyplay

import java.util.NoSuchElementException
import play.api.libs.json.Json
import play.api.mvc._
import com.wbillingsley.handy.{Ref, RefSome, RefOpt, Approval}
import Ref._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

case class Appbase[U](
  ufr:UserFromRequest[U],
  errorPF:PartialFunction[Throwable, Future[Result]] = {
    case e:NoSuchElementException => Future.successful(Results.NotFound(Json.obj("error" -> e.getMessage)))
  }
) {

  class AppbaseRequest[A](request:Request[A]) extends WrappedRequest(request) {

    lazy val user = ufr.user(request)

    lazy val approval = new Approval(user)

    val sessionKey = request.session.get("sessionKey").getOrElse(AppbaseRequest.newSessionKey)
  }

  object AppbaseRequest {
    def newSessionKey = java.util.UUID.randomUUID.toString
  }

  class UserAction(val parser: BodyParser[AnyContent])(implicit val executionContext: ExecutionContext) extends ActionBuilder[AppbaseRequest, AnyContent] with ActionTransformer[Request, AppbaseRequest] {

    def transform[A](request: Request[A]) = Future.successful {
      new AppbaseRequest(request)
    }

    def invokeBlock[A](request: Request[A])(block:AppbaseRequest[A] => Future[Result]) = {
      (for {
        r <- transform(request)
        res <- block(r)
      } yield {
        res
          .withHeaders(
            "Cache-Control" -> "no-cache, no-store, must-revalidate", "Expires" -> "0", "Pragma" -> ""
          )
          .addingToSession("sessionKey" -> r.sessionKey)(r)
      }).recoverWith(errorPF)

    }

    implicit def rtf(rr:Ref[Result]):Future[Result] = {
      rr
        .toFuture
        .recoverWith(errorPF)
    }

    implicit def rotf(rr:RefOpt[Result]):Future[Result] = {
      rr
        .orElse(RefSome(Results.NotFound(Json.obj("error" -> "Not found"))))
        .require        
        .toFuture
        .recoverWith(errorPF)
    }

  }

}
