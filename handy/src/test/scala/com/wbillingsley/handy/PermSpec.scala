package com.wbillingsley.handy

import org.specs2.mutable._
import realistic._
import Ref._
import Id._

class PermSpec extends Specification {

  case class User(id:String)

  case class Foo(id:Id[Foo,String]) extends HasStringId[Foo]

  object Foo {
    def apply(id:String):Foo = Foo(id.asId[Foo])
  }

  case class Bar(id:Id[Bar,String]) extends HasStringId[Bar]

  object Bar {
    def apply(id:String):Bar = Bar(id.asId[Bar])
  }


  // To ensure we're not actually making a lookup, we define it here so it would always fail
  implicit def LookupAlwaysFails[T]:LookUp[T,Any] = LookUp.fails[T, Any]("Look ups should not be called in this test")


  val fooPerm = Perm.of[User, Foo].onId {
    case (prior, r) => for { f <- r } yield Approved("looked it up")
  }

  val barPerm = Perm.of[User, Bar].onId {
    case (prior, r) => for { f <- r } yield Approved("looked it up")
  }


  "Permissions" should {

    "remember an approval for a Perm.onId of the same class and ID" in {

      val approval = Approval[User](RefNone)
      val foo1 = Foo("1")

      // Ask for the approval for the item itself. This should be cached.
      approval ask fooPerm(foo1.itself)

      // Then ask for it using an ID. It should not need to resolve the ID.
      approval ask fooPerm(LazyId("1").of[Foo]) must be_==(Approved("looked it up").itself)
    }

    "know that an approval for a Perm.cacheOnId with the same ID but a different generator is different" in {

      val approval = Approval[User](RefNone)
      val foo1 = Foo("1")

      // Ask for the approval for the item itself. This should be cached.
      approval ask fooPerm(foo1.itself)

      // Then ask for a Bar with the same ID.  This should fail.
      val approved = approval ask barPerm(LazyId("1").of[Bar])

      // To test the result, we use recoverWith to make the Ref recover with a known message.
      val recovered = approved recoverWith { case x:Throwable => x.getMessage().itself }

      recovered must be_==("Look ups should not be called in this test".itself)
    }

  }

}