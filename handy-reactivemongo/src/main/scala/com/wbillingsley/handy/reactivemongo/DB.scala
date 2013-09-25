package com.wbillingsley.handy.reactivemongo

import reactivemongo.api._
import reactivemongo.core.actors.Authenticate
import reactivemongo.bson._

import indexes.{Index, IndexType}
import scala.concurrent.ExecutionContext.Implicits.global

abstract class DBConnector {
  
  var dbName:String
    
  var connectionString = "localhost:27017"
    
  var dbUser:Option[String] = None
  
  var dbPwd:Option[String] = None 
  
  var useBSONIds:Boolean = true
  
  lazy val driver = new MongoDriver
    
  lazy val connection = {
    val auth = for (u <- dbUser; p <- dbPwd) yield Authenticate(dbName, u, p)
    
    driver.connection(List(connectionString), auth.toSeq)    
  }
  
  lazy val db = connection.apply(dbName)
  
  def coll(name:String) = db(name)  
  
}