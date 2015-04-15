package com.wbillingsley.handy.mongodbasync

import com.mongodb.async.client.MongoClients
import org.bson.BsonDocument

class DB {

  var dbName:String = "default_database"

  var connectionString = "mongodb://localhost:27017"

  var dbUser:Option[String] = None

  var dbPwd:Option[String] = None

  lazy val client = MongoClients.create(connectionString)

  lazy val db = client.getDatabase(dbName)

  def coll(name:String) = db.getCollection(name, classOf[BsonDocument])

}
