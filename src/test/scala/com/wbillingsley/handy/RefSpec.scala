package com.wbillingsley.handy;

import org.specs2.mutable._
import scala.concurrent.ExecutionContext.Implicits.global
import Ref._

object RefSpec extends Specification {
  
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
    
  }
  

  
}
