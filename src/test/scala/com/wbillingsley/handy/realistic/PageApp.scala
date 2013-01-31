package com.wbillingsley.handy.realistic

import com.wbillingsley.handy._
import Ref._
import WebFramework._

/*
 * Our pretend application.
 */	
object PageApp { 
  
  def setPageContent(loggedInUser:Option[Int], pageId:Option[Int], content:String):Ref[Response] = {
    val u = Ref.fromOptionId(classOf[User], loggedInUser)
    val p = Ref.fromOptionId(classOf[Page], loggedInUser)
    for {
      approval <- Approval(u) ask CanEdit(p);
      page <- p
    } yield {
      page.content = content      
      Ok("{ \"page\" : \"" + page.content + "\" }")      
    }
  }
  
  def createPage(loggedInUser:Option[Int], content:String):Ref[Response] = {
    val u = Ref.fromOptionId(classOf[User], loggedInUser)
    
    for {
      user <- optionally(u);
      approval <- Approval(user) ask CanCreate;
      page <- DB.createPage(user, content)
    } yield {
      Ok("{ \"page\" : \"" + page.content + "\" }")
    }
  }	  
  
  def viewPage(loggedInUser:Option[Int], pageId:Option[Int]):Ref[Response] = {
    val u = Ref.fromOptionId(classOf[User], loggedInUser)
    val p = Ref.fromOptionId(classOf[Page], pageId)
    
    for {
      page <- p;
      approval <- Approval(u) ask CanRead(page.itself)      
    } yield {
      Ok(page.content)
    }
  }	  
  
}