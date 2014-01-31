package com.wbillingsley.handy

import org.specs2.mutable._
import realistic._ 
import Ref._

class LookUpCacheSpec extends Specification {
      
  
  "LookUpCache" should {
    
    "cache a reference" in {      
      val cache = new LookUpCache      
      
      // Necessary to import the lookup methods
      import DB._
      
      val a = for (p <- cache(LazyId(1)[Page])) yield p.id
      a.toFutOpt must be_==(Some(1)).await
    }    
      
    "find assignable items in the cache" in {
      val cache = new LookUpCache      
 
      // Necessary to import the lookup methods
      import DB._
 
      // This should become a reference to a database class 
      val p1itself = LazyId(1).of[Page].fetch
      
      // Put it into the cache
      cache(p1itself)
      
      val assignable = classOf[Page] isAssignableFrom classOf[DB.DBPage]
      assignable must be_==(true)
      
      // Even though the class on the RefById is to the superclass, it should still find it
      cache(LazyId(1).of[Page]) must be_===(p1itself)
    }     
    
  }

}