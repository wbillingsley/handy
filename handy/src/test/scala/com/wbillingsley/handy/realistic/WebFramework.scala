package com.wbillingsley.handy.realistic

import com.wbillingsley.handy._
import Ref._

import PageApp._

/*
 * Our imaginary web framework.
 */

class Response
case class PlainResponse(code:Int, content:String) extends Response

object WebFramework {

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._

  def Ok(str:String) = PlainResponse(200, str)
  
  def route(loggedInUser: Option[Int], path: List[String], data: Map[String, String] = Map.empty): Future[Response] = {

    import ImplicitResponses._

    path match {

      case "page" :: "create" :: "POST" :: Nil => PageApp.createPage(loggedInUser, data.get("content") getOrElse "No content")
      case "page" :: id :: "GET" :: Nil => PageApp.viewPage(loggedInUser, Some(id.toInt))
      case "page" :: id :: "POST" :: Nil => PageApp.setPageContent(loggedInUser, Some(id.toInt), data.get("content") getOrElse "No content")
      case _ => Future { PlainResponse(404, "Not found") }
    }
  }

}