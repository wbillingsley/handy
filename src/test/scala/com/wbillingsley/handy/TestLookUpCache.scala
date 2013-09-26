package com.wbillingsley.handy

import org.specs2.mutable._
import realistic._ 
import Ref._

class PermissionSpec extends Specification {
      
  
  "LookUpCache" should {
    
    "cache a reference" in {      
      RefById.lookUpMethod = new RefById.LookUp {
	      override def lookup[T](r:RefById[T, _]):Ref[T] = {
	        val id = r.id match {
	          case s:String => s.toInt
	          case i:Int => i
	          case _ => -1
	        }
	        
	        DB.resolve(r.clazz, id)        
	      }
      }
      
      val cache = new LookUpCache      
      
            
      val a = for (p <- cache(new LazyId(classOf[Page], 1))) yield p.id
      a.toFuture must be_==(Some(1)).await
    }    
      
    "find assignable items in the cache" in {
      
      RefById.lookUpMethod = new RefById.LookUp {
	      override def lookup[T](r:RefById[T, _]):Ref[T] = {
	        val id = r.id match {
	          case s:String => s.toInt
	          case i:Int => i
	          case _ => -1
	        }
	        
	        DB.resolve(r.clazz, id)        
	      }
      }
      
      val cache = new LookUpCache      
      
      // This should become a reference to a database class 
      val p1itself = RefById(classOf[Page], 1).toOption.get.itself
      
      // Put it into the cache
      cache(p1itself)
      
      val assignable = classOf[Page] isAssignableFrom classOf[DB.DBPage]
      assignable must be_==(true)
      
      // Even though the class on the RefById is to the superclass, it should still find it
      cache(RefById(classOf[Page], 1)) must be_===(p1itself)       
    }     
    
  }

}