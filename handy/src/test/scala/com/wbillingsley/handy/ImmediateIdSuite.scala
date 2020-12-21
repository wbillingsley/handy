package com.wbillingsley.handy

import scala.concurrent.{ExecutionContext, Future, Promise}

class ImmediateIdSuite extends munit.FunSuite {

  case class Apple(id:String, name:String)
  case class AppleId(id:String) extends Id[Apple, String]
  
  val apples = Map(
    "1" -> Apple("1", "Royal Gala"),
    "2" -> Apple("2", "Cox's Orange Pippin"),
    "3" -> Apple("3", "Bramley"),
    "4" -> Apple("4", "Pink Lady"),
    "5" -> Apple("5", "Granny Smith")
  )

  def lookUpApple(id:AppleId):Ref[Apple] =
    if (apples.contains(id.id)) then RefItself(apples(id.id)) else RefFailed(new NoSuchElementException("Not found"))

  def lookUpOptApple(id:AppleId):RefOpt[Apple] = {
    RefOpt(apples.get(id.id))
  }
  
  given getsAppleId as GetsId[Apple, AppleId] with {
    
    override def getId[TT <: Apple](obj: TT): Option[AppleId] = Some(AppleId(obj.id))
    
    override def canonical[TT <: Apple](o: Any): Option[AppleId] = o match {
      case id:AppleId => Some(id)
      case _ => None
    }
  }

  test("If a GetsId is present, RefItself has an immediateId") {
    assertEquals(RefItself(apples("1")).immediateId, Some(AppleId("1")))
  }

  test("If a GetsId is present, LazyId has an immediateId") {
    given lookUp as EagerLookUpOne[AppleId, Apple] = (id) => lookUpApple(id)
    assertEquals(LazyId(AppleId("1")).immediateId, Some(AppleId("1")))
  }

  test("If a GetsId is present, a completed RefFuture has an immediateId") {
    val completed = Future.successful(apples("1"))
    assertEquals(RefFuture(completed)(using ExecutionContext.global).immediateId, Some(AppleId("1")))
  }

  test("If a GetsId is present, an incomplete RefFuture has no immediateId") {
    val incomplete = Promise[Apple]
    assertEquals(RefFuture(incomplete.future)(using ExecutionContext.global).immediateId, None)
  }

  test("If a GetsId is present, a completed RefFutureRef containing a completed child has an immediateId") {
    val completed = RefFutureRef(Future.successful(RefItself(apples("1"))))(using ExecutionContext.global)
    assertEquals(completed.immediateId, Some(AppleId("1")))
  }
  
  test("If a GetsId is present, a completed RefFutureRef containing an incomplete child has no immediateId") {
    given ec as ExecutionContext = ExecutionContext.global
    
    val incomplete = Promise[Apple]
    val completed = RefFutureRef(Future.successful(RefFuture(incomplete.future)))
    assertEquals(completed.immediateId, None)
  }
}
