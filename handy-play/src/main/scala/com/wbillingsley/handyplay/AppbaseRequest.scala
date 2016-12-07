package com.wbillingsley.handyplay

import java.util.NoSuchElementException
import play.api.libs.json.Json
import play.api.mvc._
import com.wbillingsley.handy.{Ref, Approval}
import Ref._

import scala.concurrent.Future

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

  object UserAction extends ActionBuilder[AppbaseRequest] with ActionTransformer[Request, AppbaseRequest] {

    import play.api.libs.concurrent.Execution.Implicits._

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
        .orIfNone(Results.NotFound(Json.obj("error" -> "Not found")).itself)
        .toFuture
        .recoverWith(errorPF)
    }

  }

}
