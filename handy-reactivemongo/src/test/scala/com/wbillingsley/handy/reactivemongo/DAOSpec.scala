package com.wbillingsley.handy.reactivemongo;


import org.specs2.mutable._
import scala.concurrent.ExecutionContext.Implicits.global
import com.wbillingsley.handy._
import com.wbillingsley.handy.Ref._
import _root_.reactivemongo.bson._



object DAOSpec {

  val testdb = new DBConnector
  
  case class Thing(id:String) extends HasStringId
  
  object ThingDAO extends DAO {
    
    type DataT = Thing
    
    val clazz = classOf[Thing]
    
    val collName = "thing"
      
    val db = testdb
    
    def unsaved = Thing("")
    
    val bsonReader = new BSONDocumentReader[Thing] {
      def read(doc:BSONDocument) = {
        Thing(doc.getAs[String]("thing").get)
      }
    }
  }
  
  case class Other(id:String) extends HasStringId
  
  object OtherDAO extends DAO {
    
    type DataT = Other
    
    val clazz = classOf[Other]
    
    val collName = "thing"
      
    val db = testdb
    
    def unsaved = Other("")
    
    val bsonReader = new BSONDocumentReader[Other] {
      def read(doc:BSONDocument) = {
        Other(doc.getAs[String]("thing").get)
      }
    }
  }
  
}

class DAOSpec extends Specification {
  
  import scala.concurrent.Future
  import DAOSpec._
  
  sequential
  
  "DataAction" should {
    
    "Loops over DAOs should be able to register their lookUps in a LookUpCatalog in a typesafe manner" in  {      
      
      val cache = new com.wbillingsley.handy.LookUpCatalog
  
      for (dao <- Seq(ThingDAO, OtherDAO)) {
        cache.registerStringLookUp(dao.clazz, dao.LookUp)
      }
      
      // If the code above in this test compiles, we're good
      1 must be equalTo 1
    }
    
  }
  
}
