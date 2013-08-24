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
      
      val rp1 = cache(RefById(classOf[Page], 1))      
      cache(RefById(classOf[Page], 1)) must be_===(rp1)       
    }    
      
    
  }

}