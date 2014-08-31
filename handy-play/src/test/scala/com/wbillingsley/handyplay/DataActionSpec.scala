package com.wbillingsley.handyplay

import com.wbillingsley.handy.Ref._
import com.wbillingsley.handy._
import play.api.libs.iteratee._
import play.api.libs.json._
import play.api.mvc._
import play.api.test._


object DataActionSpec {
  
  val fakeApp = new FakeApplication(additionalConfiguration = Map("application.secret" -> "test"))
  
  object FakeController {
  
    case class User(name:String)
  
    case class Thing(text:String)
  
    implicit object ThingToJson extends JsonConverter[Thing, User] {
      def toJson(t:Thing) = Json.obj("thing" -> t.text).itself
      def toJsonFor(t:Thing, appr:Approval[User]) = toJson(t)
    }
    
    implicit val dataActionConfig = new DataActionConfig {
      val redirectHtmlRequests = true
      def homeAction = Action { Results.Ok("") }
      def errorCodeMap = Map.empty
    }
  
    implicit object UFR extends UserFromRequest[User] {
      def user(request:RequestHeader) = User("Fred").itself
    }
    
    def json = DataAction.returning.json { 
      Json.obj("text" -> "json").itself
    }
    
    def oneThing = DataAction.returning.one { 
      Thing("one").itself
    }
    
    def threeThings = DataAction.returning.many { 
      Seq(Thing("one"), Thing("two"), Thing("three")).toRefMany
    }
    
    def jsonWithHeader = DataAction.returning.jsonWH { 
      WithHeaderInfo(
        data = Json.obj("text" -> "json").itself,
        headerInfo = HeaderInfo(headers=Seq("myHeader" -> "myValue")).itself
      )
    }
    
    def jsonFails = DataAction.returning.json {
      new RefFailed(new IllegalStateException("This is a deliberate error"))
    }
    
    def jsonFailsWithHeader = DataAction.returning.jsonWH { 
      WithHeaderInfo(
        data = new RefFailed(new IllegalStateException("This is a deliberate error")),
        headerInfo = HeaderInfo(headers=Seq("myHeader" -> "myValue")).itself
      )
    }
  }

}

class DataActionSpec extends PlaySpecification with Results {
  
  import scala.concurrent.Future
  
  def unChunk(r:Future[Result]) = {
    val str = contentAsString(r)
    val lines = str.split("\n")
    
    val buf = new StringBuffer
    if (lines.length % 2 != 0) throw new IllegalStateException("There should have been an even number of lines")
    
    var cursor = 0
    while (cursor < lines.length) {
      val line = lines(cursor + 1)
      buf.append(line.trim)
      cursor += 2
    }
    buf.toString
  }
  
  
  sequential
  
  import com.wbillingsley.handyplay.DataActionSpec._
  
  "DataAction" should {
    
    "return simple JSON objects" in new WithApplication(fakeApp) {      
      val iteratee = FakeController.json.apply(FakeRequest().withHeaders("Accept" -> "application/json"))
      val result = Enumerator.eof |>>> iteratee 
      val body = contentAsString(result)
      body must be equalTo Json.obj("text" -> "json").toString
    }
    
    "return simple JSON objects with correct headers" in new WithApplication(fakeApp) {      
      val iteratee = FakeController.jsonWithHeader.apply(FakeRequest().withHeaders("Accept" -> "application/json"))
      val result = Enumerator.eof |>>> iteratee 
      val body = contentAsString(result)
      (body must be equalTo Json.obj("text" -> "json").toString) and
      (header("myHeader", result) must be equalTo Some("myValue"))
    }
    
    "automatically convert single items for requests accepting only JSON" in new WithApplication(fakeApp) {
      val iteratee = FakeController.oneThing.apply(FakeRequest().withHeaders("Accept" -> "application/json"))
      val result = Enumerator.eof |>>> iteratee
      val body = contentAsString(result)
      body must be equalTo Json.obj("thing" -> "one").toString     
    }

    "automatically convert many items for requests accepting only JSON" in new WithApplication(fakeApp) {
      val iteratee = FakeController.threeThings.apply(FakeRequest().withHeaders("Accept" -> "application/json"))
      val result = Enumerator.eof |>>> iteratee 
      val body = unChunk(result)
      body must be equalTo Json.arr(
        Json.obj("thing" -> "one"), Json.obj("thing" -> "two"), Json.obj("thing" -> "three")
      ).toString     
    }
    
    "return a JSON error message on failure for requests accepting only JSON" in new WithApplication(fakeApp) {
      val iteratee = FakeController.jsonFails.apply(FakeRequest().withHeaders("Accept" -> "application/json"))
      val result = Enumerator.eof |>>> iteratee
      val body = contentAsString(result)
      
      (body must be equalTo Json.obj("error" -> "This is a deliberate error").toString) and
      (status(result) must be equalTo 500)
    }
    
    "return correct headers if the headerInfo is present but the data is failure" in new WithApplication(fakeApp) {      
      val iteratee = FakeController.jsonFailsWithHeader.apply(FakeRequest().withHeaders("Accept" -> "application/json"))
      val result = Enumerator.eof |>>> iteratee 
      val body = contentAsString(result)
      
      (body must be equalTo Json.obj("error" -> "This is a deliberate error").toString) and
      (status(result) must be equalTo 500) and
      (header("myHeader", result) must be equalTo Some("myValue"))
    }
    
    "return a JSON error message if a many-JSON request fails to become ready" in new WithApplication(fakeApp) {
      import com.wbillingsley.handyplay.DataActionSpec.FakeController._
      
      def method = DataAction.returning.manyJson { 
        for {
          a <- 1.itself
          f <- RefFailed(new IllegalStateException("This request should fail to become ready")):Ref[String]
          j <- Seq(Json.obj("a"->1), Json.obj("b"->2)).toRefMany
        } yield j 
      }
      
      val iteratee = method.apply(FakeRequest().withHeaders("Accept" -> "application/json"))
      val result = Enumerator.eof |>>> iteratee
      val body = contentAsString(result)
      
      (body must be equalTo Json.obj("error" -> "This request should fail to become ready").toString) and
      (status(result) must be equalTo 500)
    }

    // TODO: By default we skip errors mid-sequence; but we should introduce the option not to
    "skip failures in the middle of a sequence" in new WithApplication(fakeApp) {
      import com.wbillingsley.handyplay.DataActionSpec.FakeController._
      
      def method = DataAction.returning.manyJson { 
        for {
          i <- Seq(1, 2, 3, 4).toRefMany
          even <- if (i % 2 == 0) true.itself else RefFailed(new IllegalArgumentException("odd"))
        } yield Json.obj("even" -> even)
      }
      
      val iteratee = method.apply(FakeRequest().withHeaders("Accept" -> "application/json"))
      val result = Enumerator.eof |>>> iteratee
      val body = unChunk(result)
      
      (body must be equalTo Json.arr(
        Json.obj("even" -> true), Json.obj("even" -> true)
      ).toString) and
      (status(result) must be equalTo 200)
    }
  }
  
}
