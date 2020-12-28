package com.wbillingsley.handy

class IdSuite extends munit.FunSuite {
  
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
  
  test("Ids should be able to be looked up using an extension function if a LookUpOne is in scope") {
    given lookUp: EagerLookUpOne[AppleId, Apple] = (id) => lookUpApple(id)
    
    val id = AppleId("1")
    assertEquals(id.lookUp, RefItself(apples("1")))
  }

  test("Ids should be able to be optionally looked up using an extension function if a LookUpOpt is in scope") {
    given lookUp: EagerLookUpOpt[AppleId, Apple] = (id) => lookUpOptApple(id)

    val id = AppleId("1")
    assertEquals(id.lookUpOpt, RefSome(apples("1")))
  }

}
