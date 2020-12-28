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
  given getAppleId:GetsId[Apple, AppleId] = autoGetsId[Apple, AppleId]
  val apples = Map(
    1 -> Apple(AppleId(1), "Royal Gala"),
    2 -> Apple(AppleId(2), "Cox's Orange Pippin"),
    3 -> Apple(AppleId(3), "Bramley"),
    4 -> Apple(AppleId(4), "Pink Lady"),
    5 -> Apple(AppleId(5), "Granny Smith")
  )

  case class Orange(id:OrangeId, name:String) extends HasId[OrangeId]
  case class OrangeId(id:Int) extends Id[Orange, Int]
  given getOrangeId:GetsId[Orange, OrangeId] = autoGetsId[Orange, OrangeId]
  val oranges = Map(
    1 -> Orange(OrangeId(1), "Navel"),
    2 -> Orange(OrangeId(2), "Valencia")
  )


  /*
   * Set up some permissions.
   */

  /** Define a permission manually as a Case Class */
  case class CanEatOrange(id:OrangeId) extends Perm[User] {
    def resolve(using a:Approval[User]):Ref[Approved] = {
      for
        u <- a.who orFail Refused("Not logged in")
        even <- RefItself(u).requiring(_.id.id % 2 == 0, Refused("Odd users may not eat oranges").toRef)
      yield Approved("Even users may eat oranges")
    }
  }

  /** Define the same permission in "permission generator" style */
  val canEatOrange = Perm.onId[User, Orange, OrangeId] { (approval, rOrange) =>
    for
      u <- approval.who orFail Refused("Not logged in")
      even <- RefItself(u).requiring(_.id.id % 2 == 0, Refused("Odd users may not eat oranges").toRef)
    yield Approved("Even users may eat oranges")
  }

  /** Define a different permission using another permission generator */
  val canEatApple = Perm.onId[User, Apple, AppleId] { (approval, rApple) =>
    for
      u <- approval.who orFail Refused("Not logged in")
      a <- rApple.requiring(_.id.id % 2 != 0, Refused("Only odd apples may be eaten").toRef)
    yield Approved("Users may eat odd apples")
  }

  test("Synchronous approvals should succeed for permitted users") {
    given lookUpUser:EagerLookUpOne[UserId, User] = (uId) => RefOpt(users.get(uId.id)).require
    given a:Approval[User] = Approval(LazyId(UserId(2)).toRefOpt)

    assertEquals(a askOne CanEatOrange(OrangeId(1)), RefItself(Approved("Even users may eat oranges")))
  }

  test("Synchronous approvals should fail for non-permitted users") {
    given lookUpUser: EagerLookUpOne[UserId, User] = (uId) => RefOpt(users.get(uId.id)).require
    given a: Approval[User] = Approval(LazyId(UserId(1)).toRefOpt)

    assertEquals(a askOne CanEatOrange(OrangeId(1)), RefFailed(Refused("Odd users may not eat oranges")))
  }

  test("Synchronous approvals using permission generators should be considered equal if they have the same id and generator") {
    given lookUpUser: EagerLookUpOne[UserId, User] = (uId) => RefOpt(users.get(uId.id)).require
    given lookUpApple: EagerLookUpOne[AppleId, Apple] = (aId) => RefOpt(apples.get(aId.id)).require

    given a: Approval[User] = Approval(LazyId(UserId(1)).toRefOpt)

    // Check that the two permissions are equal. Note that we have not looked up the apple or resolved either permission.
    // We have to "for" inside the Refs, but it is synchronous in this case
    var checked = false
    for
      perm1 <- canEatApple(RefItself(apples(1)))
      perm2 <- canEatApple(LazyId(AppleId(1)))
    do {
      assertEquals(perm1, perm2)
      checked = true
    }

    assert(checked, "The equality check did not occur")
  }

  test("Synchronous approvals using permission generators should not be considered equal if they have different ids but the same generator") {
    given lookUpUser: EagerLookUpOne[UserId, User] = (uId) => RefOpt(users.get(uId.id)).require
    given lookUpApple: EagerLookUpOne[AppleId, Apple] = (aId) => RefOpt(apples.get(aId.id)).require

    given a: Approval[User] = Approval(LazyId(UserId(1)).toRefOpt)

    // Check that the two permissions are equal. Note that we have not looked up the apple or resolved either permission.
    // We have to "for" inside the Refs, but it is synchronous in this case
    var checked = false
    for
    perm1 <- canEatApple(RefItself(apples(1)))
    perm2 <- canEatApple(LazyId(AppleId(2)))
    do {
      assertNotEquals(perm1, perm2)
      checked = true
    }

    assert(checked, "The equality check did not occur")
  }

  test("Synchronous approvals using permission generators should not be considered equal if they have the same id but different generators") {
    given lookUpUser: EagerLookUpOne[UserId, User] = (uId) => RefOpt(users.get(uId.id)).require
    given lookUpApple: EagerLookUpOne[AppleId, Apple] = (aId) => RefOpt(apples.get(aId.id)).require
    given lookUpOrange: EagerLookUpOne[OrangeId, Orange] = (oId) => RefOpt(oranges.get(oId.id)).require

    given a: Approval[User] = Approval(LazyId(UserId(1)).toRefOpt)

    // Check that the two permissions are equal. Note that we have not looked up the apple or resolved either permission.
    var checked = false
    for
    perm1 <- canEatApple(RefItself(apples(1)))
    perm2 <- canEatOrange(LazyId(OrangeId(1)))
    do {
      assertNotEquals(perm1, perm2)
      checked = true
    }

    assert(checked, "The equality check did not occur")
  }

}
