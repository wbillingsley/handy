package com.wbillingsley.handy

import Ref._
import Approval._

object TestRealistic {
	/*
	 * Model classes for our sample app
	 */	
	abstract class Role
	case object Viewer extends Role
	case object Editor extends Role
	case object Admin extends Role
	
	
	/*
	 * Often you'd define these as traits so that your DB classes can implement them, but in this
	 * example we've just kept the model an d
	 */
	trait User extends HasId[Int] { val id:Int; var name:String; var roles: Set[Role] } 
	trait Page extends HasId[Int] { val id:Int; def createdBy:Ref[User]; var content:String; var isPublic:Boolean }
	
	implicit object gg extends GetsId[HasId[Int], Int] {
	    def getId(obj: HasId[Int]) = Some(obj.id)
  
	    def canonical(key:Any) = key match {
	      case k:Int => Some(k)
	      case _ => None
	    }
	}
	
	/* 
	 * Your Security classes
	 */
	case class CanEdit(page: Ref[Page]) extends PermOnIdRef[User, Page, Int](page)(gg) {
	  
	  def resolve(prior:Approval[User]):Ref[Approved] = {	    
	    (for (u <- prior.who if u.roles contains Editor) 
	      yield Approved("Is editor") 
	    ) orIfNone Refused("Is not an editor")
	  }	  
	  
	}

	case object CanCreate extends Perm[User] {
	  
	  def resolve(prior:Approval[User]):Ref[Approved] = {	    
	    (for (u <- prior.who if u.roles contains Editor) 
	      yield Approved("Is editor") 
	    ) orIfNone Refused("Is not an editor")
	  }	  
	  
	}
	
	
	/*
	 * Your DB framework, depending on what database / persistence mechanism you 
	 * use might store things quite differently.  We're just going to show this
	 * in this example by storing Page.createdBy by its user id.
	 */
  
	
	object DB {
	  
	case class DBUser(val id:Int, var name:String, var roles: Set[Role] = Set(Viewer)) extends User
	case class DBPage(val id:Int, var _createdBy:Int, var content:String, var isPublic:Boolean) extends Page {
	  
	  def createdBy = RefById(classOf[User], _createdBy)
	  
	  def createdBy_=(u: Ref[User]) {
	    for (user <- u) { _createdBy = user.id }
	  }
	  
	}
	
	  import scala.collection.mutable
	  
	  val userTable = mutable.Map(
	      1 -> DBUser(1, "An admin", Set(Viewer, Editor, Admin)),
	      2 -> DBUser(2, "An editor", Set(Viewer, Editor)),
	      3 -> DBUser(3, "A viewer", Set(Viewer))
	  )
	  
	  val pageTable = mutable.Map(
		  1 -> DBPage(1, 1, "A public page", true),
		  2 -> DBPage(2, 1, "A non-public page", true)
	  )
	  
	  def createPage(createdBy:Int, content:String, isPublic:Boolean) = {
	    val key = pageTable.keySet.max + 1	    
	    val p = DBPage(key, createdBy, content, isPublic)
	    pageTable.put(key, p)
	    p itself	    
	  }
	  
	}

	
	/*
	 * Now we write the API endpoint for your business logic
	 */	
	object PageApp { 
	  
	  def setPageContent(loggedInUser:Option[Int], pageId:Option[Int], content:String):Ref[Page] = {
	    val u = Ref.fromOptionId(classOf[User], loggedInUser)
	    val p = Ref.fromOptionId(classOf[Page], loggedInUser)
	    for {
	      approval <- Approval(u) ask CanEdit(p);
	      page <- p
	    } yield {
	      page.content = content
	      page
	    }
	  }
	  
	  def createPage(loggedInUser:Option[Int], content:String):Ref[Page] = {
	    val u = Ref.fromOptionId(classOf[User], loggedInUser)
	    val p = Ref.fromOptionId(classOf[Page], loggedInUser)
	    
	    val oiu = (Approval(u) ask CanEdit(p)).flatMap { approval => 
	      p
	    }
	    
	    for {
	      approval <- Approval(u) ask CanEdit(p);
	      page <- p
	    } yield {
	      page.content = content
	      page
	    }
	  }	  
	  
	}
	

    class Response
	case class PlainResponse(code:Int, content:String) extends Response
	
	
	/*
	 * Our implicit conversions for turning things into a response
	 */
	
	object ImplicitResponses {
	  
	  import scala.concurrent._
	  import scala.concurrent.ExecutionContext.Implicits._

	  implicit def refPageToFutResponse(rp: Ref[Page]):Future[Response] = {
	    val p = promise[Response]
	    
	    rp onComplete (
	      onSuccess = { page => p success PlainResponse(200, page.toString) },
	      onNone = p success PlainResponse(404, "Not found"),
	      onFail = _ match {
	        case Refused(msg) => p success PlainResponse(403, msg)
	        case t:Throwable => p success PlainResponse(500, t.getMessage)
	      }	      
	    )
	    
	    p.future
	    
	  }
	  
	}
	
	
	/*
	 * Our imaginary web framework
	 */
	object DumbWebFramework {
	  
	  import scala.concurrent._
	  import scala.concurrent.ExecutionContext.Implicits._
	  
	  
	  def route(loggedInUser: Option[Int], path:List[String], data:Map[String,String]):Future[Response] = {
	    
	    import ImplicitResponses._
	    
	    path match {
	      
	      case "page" :: "create" :: Nil => PageApp.createPage(loggedInUser, data.get("content") getOrElse "No content")
	      case _ => future { PlainResponse(404, "Not found") }
	    }
	  }
	  
	  
	  
	  
	}
	
	
	
}


import TestRealistic._


class TestRealistic {

}