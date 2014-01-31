package com.wbillingsley.handy

import org.specs2.mutable._
import realistic._
import Ref._

class PermSpec extends Specification {

  case class User(id:String)

  case class Foo(id:String) extends HasStringId

  case class Bar(id:String) extends HasStringId

  // To ensure we're not actually making a lookup, we define it here so it would always fail
  implicit def LookupAlwaysFails[T, K] = LookUp.fails[T, K]("Look ups should not be called in this test")

  case class FooPerm(r:Ref[Foo]) extends PermOnIdRef[User, Foo](r) {
    def resolve(prior:Approval[User]) = for { f <- r } yield Approved("looked it up")
  }

  case class BarPerm(r:Ref[Bar]) extends PermOnIdRef[User, Bar](r) {
    def resolve(prior:Approval[User]) = for { f <- r } yield Approved("looked it up")
  }

  val fooCache = Perm.of[User, Foo].cacheOnId {
    case (prior, r) => for { f <- r } yield Approved("looked it up")
  }

  val barCache = Perm.of[User, Bar].cacheOnId {
    case (prior, r) => for { f <- r } yield Approved("looked it up")
  }


  "Permissions" should {

    "remember an approval for a PermOnId of the same class and ID" in {

      val approval = Approval[User](RefNone)
      val foo1 = Foo("1")

      // Ask for the approval for the item itself. This should be cached.
      approval ask FooPerm(foo1.itself)

      // Then ask for it using an ID. It should not need to resolve the ID.
      approval ask FooPerm(LazyId("1").of[Foo]) must be_==(Approved("Already approved").itself)
    }

    "know that an approval for a PermOnId with the same ID but a different permission class is different" in {

      val approval = Approval[User](RefNone)
      val foo1 = Foo("1")

      // Ask for the approval for the item itself. This should be cached.
      approval ask FooPerm(foo1.itself)

      // Then ask for a Bar with the same ID.  This should fail.
      val approved = approval ask BarPerm(LazyId("1").of[Bar])

      // To test the result, we use recoverWith to make the Ref recover with a known message.
      val recovered = approved recoverWith { case x:Throwable => x.getMessage().itself }

      recovered must be_==("Look ups should not be called in this test".itself)
    }

    "remember an approval for a Perm.cacheOnId of the same class and ID" in {

      val approval = Approval[User](RefNone)
      val foo1 = Foo("1")

      // Ask for the approval for the item itself. This should be cached.
      approval ask fooCache(foo1.itself)

      // Then ask for it using an ID. It should not need to resolve the ID.
      approval ask fooCache(LazyId("1").of[Foo]) must be_==(Approved("Already approved").itself)
    }

    "know that an approval for a Perm.cacheOnId with the same ID but a different generator is different" in {

      val approval = Approval[User](RefNone)
      val foo1 = Foo("1")

      // Ask for the approval for the item itself. This should be cached.
      approval ask fooCache(foo1.itself)

      // Then ask for a Bar with the same ID.  This should fail.
      val approved = approval ask barCache(LazyId("1").of[Bar])

      // To test the result, we use recoverWith to make the Ref recover with a known message.
      val recovered = approved recoverWith { case x:Throwable => x.getMessage().itself }

      recovered must be_==("Look ups should not be called in this test".itself)
    }

  }

}