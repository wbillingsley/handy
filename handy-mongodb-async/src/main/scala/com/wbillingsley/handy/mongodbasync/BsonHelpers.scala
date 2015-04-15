package com.wbillingsley.handy.mongodbasync

import com.wbillingsley.handy._
import org.bson.{BsonDocument, BsonValue, BsonString, BsonObjectId}
import org.bson.types.ObjectId

import scala.language.implicitConversions

object BsonHelpers {

  implicit def idToBson(id:Id[_, String]) = new BsonObjectId(new ObjectId(id.id))

  implicit def stringToBson(s:String) = new BsonString(s)

  implicit def mapToDoc(m:Map[String, BsonValue]) = {
    val d = new BsonDocument()
    for { (k, v) <- m } d.append(k,v)
    d
  }

  def bsonDoc(tuples:(String,BsonValue)*) = {
    val d = new BsonDocument()
    for { (k, v) <- tuples } d.append(k,v)
    d
  }

}
