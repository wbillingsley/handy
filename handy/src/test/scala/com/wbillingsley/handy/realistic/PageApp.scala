package com.wbillingsley.handy.realistic

import com.wbillingsley.handy._
import Ref._
import Id._
import WebFramework._

/*
 * Our pretend application.
 */	
object PageApp { 
  
  // Import the lookUp methods from the database
  import DB._
  import Security._
  
  def setPageContent(loggedInUser:Option[Int], pageId:Option[Int], content:String):Ref[Response] = {
    val u = RefOpt(loggedInUser).flatMap(_.asId[User].lazily)
    val p = RefOpt(pageId).flatMap(_.asId[Page].lazily).require
    for {
      approval <- Approval(u) ask canEdit(p);
      page <- p
    } yield {
      page.content = content      
      Ok("{ \"page\" : \"" + page.content + "\" }")      
    }
  }
  
  def createPage(loggedInUser:Option[Int], content:String):Ref[Response] = {
    val u = RefOpt(loggedInUser).flatMap(_.asId[User].lazily)
    
    for {
      approval <- Approval(u) ask canCreate;
      user <- u.require
      page <- DB.createPage(user.itself, content)
    } yield {
      Ok("{ \"page\" : \"" + page.content + "\" }")
    }
  }	  
  
  def viewPage(loggedInUser:Option[Int], pageId:Option[Int]):Ref[Response] = {
    val u = RefOpt(loggedInUser).flatMap(_.asId[User].lazily)
    val p = RefOpt(pageId).flatMap(_.asId[Page].lazily).require
    
    for {
      page <- p;
      approval <- Approval(u) ask canRead(page.itself)
    } yield {
      Ok(page.content)
    }
  }	  
  
}