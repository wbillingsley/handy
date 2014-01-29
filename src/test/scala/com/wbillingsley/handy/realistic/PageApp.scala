package com.wbillingsley.handy.realistic

import com.wbillingsley.handy._
import Ref._
import WebFramework._

/*
 * Our pretend application.
 */	
object PageApp { 
  
  // Import the lookUp methods from the database
  import DB._
  
  def setPageContent(loggedInUser:Option[Int], pageId:Option[Int], content:String):Ref[Response] = {
    val u = Ref.fromOptionId[User, Int](loggedInUser)
    val p = Ref.fromOptionId[Page, Int](loggedInUser)
    for {
      approval <- Approval(u) ask CanEdit(p);
      page <- p
    } yield {
      page.content = content      
      Ok("{ \"page\" : \"" + page.content + "\" }")      
    }
  }
  
  def createPage(loggedInUser:Option[Int], content:String):Ref[Response] = {
    val u = Ref.fromOptionId[User, Int](loggedInUser)
    
    for {
      user <- optionally(u);
      approval <- Approval(user) ask CanCreate;
      page <- DB.createPage(user, content)
    } yield {
      Ok("{ \"page\" : \"" + page.content + "\" }")
    }
  }	  
  
  def viewPage(loggedInUser:Option[Int], pageId:Option[Int]):Ref[Response] = {
    val u = Ref.fromOptionId[User, Int](loggedInUser)
    val p = Ref.fromOptionId[Page, Int](pageId)
    
    for {
      page <- p;
      approval <- Approval(u) ask CanRead(page.itself)      
    } yield {
      Ok(page.content)
    }
  }	  
  
}