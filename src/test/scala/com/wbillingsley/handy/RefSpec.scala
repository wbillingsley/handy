package com.wbillingsley.handy;

import org.specs2.mutable._
import scala.concurrent.ExecutionContext.Implicits.global
import Ref._

object RefSpec extends Specification {
  
  case class TItem(id: Long, name: String)
  
  /*
   * Mimics a synchronous database
   */
  class SynchronousDB {
    val objMap = scala.collection.mutable.Map.empty[Long, TItem]

    implicit val lum = new LookUpOne[TItem, Long] {
      def lookUpOne(r:RefById[TItem, Long]) = {
        objMap.get(r.id) match {
          case Some(x) => RefItself(x)
          case _ => RefNone 
        }
      }
    }
  }
  
  "Ref" should {
    
    "support 'for' syntactic sugar across Option" in {
      val rfoo = "foo".itself
      val optSome = Some(1)
      
      val rRes = for {
        foo <- rfoo
        num <- optSome toRef;
        foo2 <- rfoo
      } yield foo.length + num + foo2.length
      
      rRes must be_==(7.itself)
    }
    
    "support 'for' syntactic sugar across Try" in {
      
      import scala.util.{Try, Success, Failure}
      
      val rfoo = "foo".itself
      val optTry = Try { 1 }
      
      val rRes = for {
        foo <- rfoo
        num <- optTry toRef;
        foo2 <- rfoo
      } yield foo.length + num + foo2.length
      
      rRes must be_==(7.itself)
    }
    
    
    "support GetId for objects extending HasStringId" in {
      
      case class MyFoo(id:String, foo:String) extends HasStringId
      
      val foo1 = MyFoo("1", "foo")
      
      val foo1itself = foo1.itself
      
      foo1itself.getId must be_==(Some("1"))
    }
    
    "support successful synchronous lookups" in {
      val db = new SynchronousDB

      // Put an item in the database
      val item = TItem(1, "one")
      db.objMap.put(1, item)
      
      // import the implicit lookUp method
      import db.lum
    
      RefById(classOf[TItem], 1L).fetch must be equalTo RefItself(TItem(1, "one"))
    }

    "support unsuccessful synchronous lookups" in {
      val db = new SynchronousDB

      // Put an item in the database
      val item = TItem(1, "one")
      db.objMap.put(1, item)
      
      // import the implicit lookUp method
      import db.lum
    
      RefById(classOf[TItem], 2L).fetch must be equalTo RefNone
    }    
  }
  

  
}
