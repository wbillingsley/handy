package com.wbillingsley.handy;

import org.specs2.mutable._

import scala.concurrent.ExecutionContext.Implicits.global
import Ref._
import org.specs2.concurrent.ExecutionEnv

class RefSpec(implicit ee: ExecutionEnv) extends Specification {
  
  case class TItem(id: Long, name: String)
  
  /*
   * Mimics a synchronous database
   */
  class SynchronousDB {
    val objMap = scala.collection.mutable.Map.empty[Long, TItem]

    implicit val lum = new LookUp[TItem, Long] {
      def one[K <: Long](r:Id[TItem, K]):Ref[TItem] = {
        objMap.get(r.id) match {
          case Some(x) => RefItself(x)
          case _ => RefFailed(new NoSuchElementException("Nothing to look up"))
        }
      }

      def many[K <: Long](r:Ids[TItem, K]):RefMany[TItem] = {
        import Id._
        for {
          id <- r.ids.toRefMany
          item <- one(id.asId[TItem])
        } yield item
      }
    }

  }
  
  "Ref" should {

    "support flatMap redirection OneToOne" in {
      def x(i:Int):Ref[Int] = RefItself(i)

      x(1) flatMap x must be_==(x(1))
    }

    "support flatMap redirection OneToOpt" in {
      def x(i:Int):Ref[Int] = RefItself(i)
      def xo(i:Int):RefOpt[Int] = RefSome(i)

      x(1) flatMap xo must be_==(xo(1))
    }

    "support flatMap redirection OneToNone" in {
      def x(i:Int):Ref[Int] = RefItself(i)
      def xo(i:Int) = RefNone

      x(1) flatMap xo must be_==(xo(1))
    }

    "support flatMap redirection OneToMany" in {
      def x(i:Int):Ref[Int] = RefItself(i)
      def xm(i:Int):RefMany[Int] = RefIterableOnce(Seq.fill(i)(i))

      x(1) flatMap xm must be_==(xm(1))
    }

    "support 'for' syntactic sugar across Option" in {
      val rfoo = "foo".itself
      val optSome = Some(1)
      
      val rRes = for {
        foo <- rfoo
        num <- optSome.toRef
        foo2 <- rfoo
      } yield foo.length + num + foo2.length
      
      rRes.require.toFuture must be_==(7).await
    }
    
    "support 'for' syntactic sugar across Try" in {
      
      import scala.util.{Try, Success, Failure}
      
      val rfoo = "foo".itself
      val optTry = Try { 1 }
      
      val rRes = for {
        foo <- rfoo
        num <- optTry.toRef
        foo2 <- rfoo
      } yield foo.length + num + foo2.length
      
      rRes.toFuture must be_==(7).await
    }
    
    
    "support GetId for objects extending HasStringId" in {

      import Id._

      case class MyFoo(_id:String, foo:String) extends HasStringId[MyFoo] {
        def id = _id.asId[MyFoo]
      }
      
      val foo1 = MyFoo("1", "foo")
      
      val foo1itself = foo1.itself
      
      foo1itself.refId.require.toFuture must be_==("1".asId[MyFoo]).await
    }
    
    "support successful synchronous lookups" in {
      val db = new SynchronousDB

      // Put an item in the database
      val item = TItem(1, "one")
      db.objMap.put(1, item)
      
      // import the implicit lookUp method
      import db.lum
      val a = LazyId(1L).apply[TItem]
      a.lookUp.toFuture must be_==(TItem(1, "one")).await
    }

    "support unsuccessful synchronous lookups" in {
      val db = new SynchronousDB

      // Put an item in the database
      val item = TItem(1, "one")
      db.objMap.put(1, item)
      
      // import the implicit lookUp method
      import db.lum
    
      LazyId.of[TItem](2L).toFuture must throwAn[NoSuchElementException].await
    }

    "implicitly find a lookup that takes a supertype of the key" in {

      trait MyId {
        def canonical:String
      }
      case class IntId(k:Int) extends MyId {
        def canonical:String = k.toString
      }
      case class StringId(s:String) extends MyId {
        def canonical:String = s
      }

      // a map with some dummy data
      val objMap = scala.collection.mutable.Map("1" -> TItem(1L, "one"))

      // a LookUp that accepts the superclass
      implicit val lum = new LookUp[TItem, MyId] {
        def one[K <: MyId](r:Id[TItem, K]):Ref[TItem] = {
          objMap.get(r.id.canonical) match {
            case Some(x) => RefItself(x)
            case _ => RefFailed(new NoSuchElementException)
          }
        }

        def many[K <: MyId](r:Ids[TItem, K]):RefMany[TItem] = {
          for { id <- r.toSeqId.toRefMany; item <- one(id) } yield item
        }
      }

      /*
       * create refs using both notations. If these compile, the implicit has been found
       */
      import Id._
      val oneStr = LazyId.of[TItem](StringId("1"))
      val oneInt = IntId(1).asId[TItem]

      // Just check they resolve the same
      val check = for {
        s <- oneStr.lookUp
        i <- oneInt.lookUp
      } yield s == i

      check.toFuture must be_==(true).await
    }

    "Support empty look ups" in {
      var foo:LazyId[TItem, _] = LazyId.empty
      foo.toRefOpt.toFutureOpt must be_==(None).await
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
      rm.collect.toFuture must be_==(
        List(TItem(1, "one"), TItem(2, "two"), TItem(3, "three"), TItem(4, "four"))
      ).await
    }

  }
  

  
}
