package com.wbillingsley.handy.reactivemongo

import reactivemongo.api._
import reactivemongo.core.nodeset.Authenticate
import reactivemongo.bson._

import indexes.{Index, IndexType}
import scala.concurrent.ExecutionContext.Implicits.global

class DBConnector {
  
  var dbName:String = "default_database"
    
  var connectionString = "localhost:27017"
    
  var dbUser:Option[String] = None
  
  var dbPwd:Option[String] = None 
  
  var useBSONIds:Boolean = true
  
  lazy val driver = new MongoDriver
    
  lazy val connection = {
    val auth = for (u <- dbUser; p <- dbPwd) yield Authenticate(dbName, u, p)
    
    driver.connection(
      nodes=List(connectionString),
      authentications=auth.toSeq
    )
  }
  
  lazy val db = connection.apply(dbName)
  
  def coll(name:String) = db(name)  
  
}