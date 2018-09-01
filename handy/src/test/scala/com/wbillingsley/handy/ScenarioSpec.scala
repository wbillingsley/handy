package com.wbillingsley.handy

import Ref._
import Approval._
import com.wbillingsley.handy.realistic._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable._

/**
 * Tests our mocked up web application in com.wbillingsley.handy.realistic works.
 * 
 * See the code in src/test/com/wbillingsley/handy/realistic for how an App can be written
 */
class ScenarioSpec(implicit ee: ExecutionEnv) extends Specification {

  import scala.concurrent._
  import scala.concurrent.duration._


  "Our editor scenario" should {
    "create a page" in {
      // This should succeed as user #2 is an editor
      val request = WebFramework.route(
        loggedInUser = Some(2), // An editor
        path = "page" :: "create" :: "POST" :: Nil,
        data = Map("content" -> "hello world")
      )

      request must be_==(PlainResponse(200, "{ \"page\" : \"hello world\" }")).await
    }

    "not allow non-editors to create pages" in {
      // This should fail as user #3 is not an editor
      val request = WebFramework.route(
        loggedInUser = Some(3), // NOT an editor
        path = "page" :: "create" :: "POST" :: Nil,
        data = Map("content" -> "hello world")
      )

      request must be_==(PlainResponse(403, "You do not have the role Editor")).await
    }

    "let anonymous users see public pages" in {

      // This should succeed as page #1 is public
      val request = WebFramework.route(
        loggedInUser = None, // Not logged in
        path = "page" :: "1" :: "GET" :: Nil
      )

      request must be_==(PlainResponse(200, "A public page")).await
    }

    "not let anonymous users view non-public pages" in {

      // This should fail as page #2 is not public
      val request = WebFramework.route(
        loggedInUser = None, // Not logged in
        path = "page" :: "2" :: "GET" :: Nil
      )

      request must be_==(PlainResponse(403, "You do not have the role Viewer")).await
    }

    "let viewers see non-public pages" in {
      // This should succeed as user #3 is a viewer
      val request = WebFramework.route(
        loggedInUser = Some(3), // A viewer
        path = "page" :: "2" :: "GET" :: Nil
      )

      request must be_==(PlainResponse(200, "A non-public page")).await
    }

  }
  

  

  
}

