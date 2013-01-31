package com.wbillingsley.handy

import Ref._
import Approval._

import com.wbillingsley.handy.realistic._

import org.junit.Assert._
import org.junit._
import scala.concurrent.ExecutionContext.Implicits.global


object TestRealistic {
  
  val lum = new RefById.LookUp {
      
      override def lookup[T](r:RefById[T, _]):Ref[T] = {
        val id = r.id match {
          case s:String => s.toInt
          case i:Int => i
          case _ => -1
        }
        
        DB.resolve(r.clazz, id)        
      }
    
  }
  
}

/**
 * Tests our mocked up web application in com.wbillingsley.handy.realistic works.
 * 
 * See the code in src/test/com/wbillingsley/handy/realistic for how an App can be written
 */
class TestRealistic {

  import scala.concurrent._
  import scala.concurrent.duration._
  
  @Before def before {
    RefById.lookUpMethod = TestRealistic.lum
  }

  @Test def testEditorCreateSucceeds {
    
    // This should succeed as user #2 is an editor
    val request = WebFramework.route(
        loggedInUser = Some(2), // An editor
        path = "page" :: "create" :: "POST" :: Nil,
        data = Map("content" -> "hello world")        
    )
    
    val result = Await.result(request, 1.second)
    assertEquals(PlainResponse(200, "{ \"page\" : \"hello world\" }"), result)
  }
  
  @Test def testNonEditorCreateFails = {
    
    // This should fail as user #3 is not an editor 
    val request = WebFramework.route(
        loggedInUser = Some(3), // NOT an editor
        path = "page" :: "create" :: "POST" :: Nil,
        data = Map("content" -> "hello world")        
    )
    
    val result = Await.result(request, 1.second)
    assertEquals(PlainResponse(403, "You do not have the role Editor"), result)    
        
  }
  
  /**
   * Make some HTTP requests to /page/create
   */
  @Test def testAnonViewPublicSucceeds = {
    
    // This should succeed as page #1 is public
    val request = WebFramework.route(
        loggedInUser = None, // Not logged in
        path = "page" :: "1" :: "GET" :: Nil        
    )
    
    val result = Await.result(request, 1.second)
    assertEquals(PlainResponse(200, "A public page"), result)

  }
  
  @Test def testAnonViewNonPublicFails = {
    
    // This should fail as page #2 is not public
    val request = WebFramework.route(
        loggedInUser = None, // Not logged in
        path = "page" :: "2" :: "GET" :: Nil        
    )
    
    val result = Await.result(request, 1.second)
    assertEquals(PlainResponse(403, "You do not have the role Viewer"), result)

  }
  
  @Test def testViewerViewNonPublicSucceeds = {
    // This should succeed as user #3 is a viewer
    val request = WebFramework.route(
        loggedInUser = Some(3), // A viewer
        path = "page" :: "2" :: "GET" :: Nil        
    )
    
    val result = Await.result(request, 1.second)
    assertEquals(PlainResponse(200, "A non-public page"), result)

  }
  
}

