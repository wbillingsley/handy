package com.wbillingsley.handy

import com.wbillingsley.handy.Ref._
import com.wbillingsley.handy.realistic._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable._

class LookUpCacheSpec(implicit ee: ExecutionEnv) extends Specification {
      
  
  "LookUpCache" should {

    // Necessary to import the lookup methods
    import DB._

    "cache a reference" in {
      val cache = new LookUpCache      

      val refOne = LazyId(1).of[Page]

      val a = for (p <- cache(refOne)) yield p.id.id
      a.toFuture must be_==(1).await
    }    
      
    "find assignable items in the cache" in {
      val cache = new LookUpCache      
 
      // Necessary to import the lookup methods
      import DB._
 
      // This should become a reference to a database class 
      val p1itself = LazyId(1).of[Page].lookUp
      
      // Put it into the cache
      cache(p1itself)
      
      val assignable = classOf[Page] isAssignableFrom classOf[DB.DBPage]
      assignable must be_==(true)
      
      // Even though the class on the RefById is to the superclass, it should still find it
      cache(LazyId(1).of[Page]) must be_==(p1itself)
    }     
    
  }

}