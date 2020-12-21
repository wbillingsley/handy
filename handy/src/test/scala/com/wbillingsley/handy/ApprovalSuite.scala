package com.wbillingsley.handy

class ApprovalSuite extends munit.FunSuite {

  /*
   * Set up some classes for this test suite.
   * In this case, we'll use classes that have typed Ids
   */
  
  case class User(id:UserId, name:String) extends HasId[UserId]
  case class UserId(id:Int) extends Id[User, Int]
  val users = Map(
    1 -> User(UserId(1), "Algernon"),
    2 -> User(UserId(2), "Bertie"),
    3 -> User(UserId(3), "Cedric"),
  )
  
  case class Apple(id:AppleId, name:String) extends HasId[AppleId]
  case class AppleId(id:Int) extends Id[Apple, Int]
  
  case class Orange(id:OrangeId, name:String) extends HasId[OrangeId]
  case class OrangeId(id:Int) extends Id[Orange, Int]

  /*
   * Set up some permissions. 
   */
  case class CanEatOrange(id:OrangeId) extends Perm[User] {
    def resolve(using a:Approval[User]):Ref[Approved] = { 
      for 
        u <- a.who orFail Refused("Not logged in")
        even <- RefItself(u).withFilter(_.id.id % 2 == 0) orFail Refused("Odd users may not eat oranges")
      yield Approved("Even users may eat oranges") 
    }
  } 
  
  test("Synchronous approvals should succeed for permitted users") {
    given lookUpUser as EagerLookUpOne[UserId, User] = (uId) => RefOpt(users.get(uId.id)).require
    given a as Approval[User] = Approval(LazyId(UserId(2)).toRefOpt)
    
    assertEquals(a askOne CanEatOrange(OrangeId(1)), RefItself(Approved("Even users may eat oranges")))
  }

  test("Synchronous approvals should fail for non-permitted users") {
    given lookUpUser as EagerLookUpOne[UserId, User] = (uId) => RefOpt(users.get(uId.id)).require
    given a as Approval[User] = Approval(LazyId(UserId(1)).toRefOpt)

    assertEquals(a askOne CanEatOrange(OrangeId(1)), RefFailed(Refused("Odd users may not eat oranges")))
  }

  
}
