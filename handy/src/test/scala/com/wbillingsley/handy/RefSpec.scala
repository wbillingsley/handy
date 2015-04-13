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

    implicit val lum = new LookUp[TItem, Long] {
      def one[K <: Long](r:Id[TItem, K]) = {
        objMap.get(r.id) match {
          case Some(x) => RefItself(x)
          case _ => RefNone 
        }
      }

      def many[K <: Long](r:Ids[TItem, K]) = {
        import Id._
        r.ids.map(_.asId[TItem]).toRefMany flatMap one
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

      import Id._

      case class MyFoo(_id:String, foo:String) extends HasStringId[MyFoo] {
        def id = _id.asId[MyFoo]
      }
      
      val foo1 = MyFoo("1", "foo")
      
      val foo1itself = foo1.itself
      
      foo1itself.getId must be_==(Some("1".asId[MyFoo]))
    }
    
    "support successful synchronous lookups" in {
      val db = new SynchronousDB

      // Put an item in the database
      val item = TItem(1, "one")
      db.objMap.put(1, item)
      
      // import the implicit lookUp method
      import db.lum
      val a = LazyId(1L).apply[TItem]
      a.fetch must be equalTo RefItself(TItem(1, "one"))
    }

    "support unsuccessful synchronous lookups" in {
      val db = new SynchronousDB

      // Put an item in the database
      val item = TItem(1, "one")
      db.objMap.put(1, item)
      
      // import the implicit lookUp method
      import db.lum
    
      LazyId.of[TItem](2L).fetch must be equalTo RefNone
    }

    "implicitly find a lookup that takes a supertype of the key" in {

      trait MyId {
        def canonical:String
      }
      case class IntId(val k:Int) extends MyId {
        def canonical = k.toString
      }
      case class StringId(val s:String) extends MyId {
        def canonical = s
      }

      // a map with some dummy data
      val objMap = scala.collection.mutable.Map("1" -> TItem(1L, "one"))

      // a LookUp that accepts the superclass
      implicit val lum = new LookUp[TItem, MyId] {
        def one[K <: MyId](r:Id[TItem, K]) = {
          objMap.get(r.id.canonical) match {
            case Some(x) => RefItself(x)
            case _ => RefNone
          }
        }

        def many[K <: MyId](r:Ids[TItem, K]) = {
          import Id._
          r.ids.map(_.asId[TItem]).toRefMany flatMap one
        }
      }

      /*
       * create refs using both notations. If these compile, the implicit has been found
       */
      val oneStr = LazyId.of[TItem](StringId("1"))
      val oneInt = LazyId(IntId(1))[TItem]

      // Just check they resolve the same
      oneStr.fetch must be equalTo oneInt.fetch
    }

    "Support empty look ups" in {
      var foo:LazyId[TItem, _] = LazyId.empty
      foo.fetch must be_==(RefNone)
    }

    "Support lookups of RefManys" in {
      val db = new SynchronousDB

      // Put items in the database
      db.objMap.put(1, TItem(1, "one"))
      db.objMap.put(2, TItem(2, "two"))
      db.objMap.put(3, TItem(3, "three"))
      db.objMap.put(4, TItem(4, "four"))

      // import the implicit lookUp method
      import db.lum

      val str = Seq(1L, 2L, 3L, 4L)

      val rm = RefManyById(str).of[TItem]
      rm.fetch.toList must be equalTo List(TItem(1, "one"), TItem(2, "two"), TItem(3, "three"), TItem(4, "four"))
    }

  }
  

  
}
