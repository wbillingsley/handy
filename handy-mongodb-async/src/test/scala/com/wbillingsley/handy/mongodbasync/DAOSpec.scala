package com.wbillingsley.handy.mongodbasync

import com.mongodb.WriteConcern
import com.mongodb.client.model.Filters
import com.wbillingsley.handy._
import org.bson.{BsonObjectId, BsonDocument}
import org.bson.types.ObjectId
import org.specs2.mutable.Specification
import play.api.libs.iteratee.Iteratee

import scala.util.Try

import BsonHelpers._


object DAOSpec {

  val testdb = new DB

  case class Thing(id:Id[Thing, String], value:String) extends HasStringId[Thing]

  val bsonConverter = new BsonDocumentConverter[Thing] {
    def read(doc:BsonDocument) = Try {
      Thing(
        id = Id(doc.getObjectId("_id").getValue.toHexString),
        value = doc.getString("value").getValue
      )
    }

    def write(t:Thing) = bsonDoc(
      "_id" -> t.id,
      "value" -> t.value
    )
  }

  object ThingDAO extends DAO(
    db = testdb,
    clazz = classOf[Thing],
    collName = "thing"
  ) (
    executionContext = scala.concurrent.ExecutionContext.Implicits.global,
    converter = bsonConverter
  )

}

class DAOSpec extends Specification {

  import DAOSpec._

  sequential

  "DataAction" should {

    "Retrieve data it puts" in  {
      val t1 = Thing(Id(ThingDAO.allocateId), "first")
      (for {
        dropped <- FuturifySRC.void(ThingDAO.coll.withWriteConcern(WriteConcern.FSYNCED).drop(_))
        saved <- ThingDAO.saveAndFetch(t1).toFuture
      } yield saved.value) must beEqualTo("first").await
    }

    "Retrieve mutliple it puts" in  {
      val t1 = Thing(Id(ThingDAO.allocateId), "first")
      val t2 = Thing(Id(ThingDAO.allocateId), "first")

      val feiAll = (for {
        dropped <- FuturifySRC.void(ThingDAO.coll.withWriteConcern(WriteConcern.FSYNCED).drop(_))
        saved1 <- ThingDAO.saveSafe(t1).toFuture
        saved2 <- ThingDAO.saveSafe(t2).toFuture
        all <- ThingDAO.feiFindMany(Filters.eq("value", "first"))
      } yield all)

      ThingDAO.refMany(feiAll).collect.map(_.length).toFuture must beEqualTo(2).await
    }
  }

}
