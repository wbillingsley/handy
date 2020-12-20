package com.wbillingsley.handy.realistic

import com.wbillingsley.handy.Ref._
import com.wbillingsley.handy._
import com.wbillingsley.handy.realistic.PageApp._

/*
 * Our imaginary web framework.
 */

class Response
case class PlainResponse(code:Int, content:String) extends Response

object WebFramework {

  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent._

  def Ok(str:String) = PlainResponse(200, str)
  
  def route(loggedInUser: Option[Int], path: List[String], data: Map[String, String] = Map.empty): Future[Response] = {

    import ImplicitResponses._

    (path match {
      case "page" :: "create" :: "POST" :: Nil => PageApp.createPage(loggedInUser, data.getOrElse("content", "No content"))
      case "page" :: id :: "GET" :: Nil => PageApp.viewPage(loggedInUser, Some(id.toInt))
      case "page" :: id :: "POST" :: Nil => PageApp.setPageContent(loggedInUser, Some(id.toInt), data.getOrElse("content", "No content"))
      case _ => PlainResponse(404, "Not found").itself
    }).recoverWith {
      case Refused(x) => PlainResponse(403, x).itself
      case x:Throwable => PlainResponse(500, x.getMessage).itself
    }.toFuture
  }

}