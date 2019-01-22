package com.wbillingsley.handy

import Ref._
import org.specs2.mutable._

object ApprovalSpec {
  case class User(name:String)

  case class CanDoOdd(i: Int) extends Perm[User] {

    def resolve(prior: Approval[User]) = {
      if (i % 2 ==1) {
        Approved("Yes, it was odd")
      } else {
        Refused("No, it was even")
      }
    }

  }

}



class ApprovalSpec extends Specification {

  import ApprovalSpec._

  val fred = User("fred")
  
  "Approval" should {
    "Approve a simple request" in {
      val a = Approval(fred.itself.toRefOpt)

      a ask CanDoOdd(1) must be_==(RefItself(Approved("Yes, it was odd")))
    }
  }

}