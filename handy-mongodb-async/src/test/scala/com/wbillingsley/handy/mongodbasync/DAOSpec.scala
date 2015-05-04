package com.wbillingsley.handy.mongodbasync

import com.mongodb.WriteConcern
import com.mongodb.client.model.Filters
import com.wbillingsley.handy._
import Ref._
import com.wbillingsley.handy.mongodbasync.BsonHelpers._
import org.bson.BsonDocument
import org.specs2.mutable.Specification
import play.api.libs.iteratee.Iteratee

import scala.util.Try


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

    "Retrieve more than 101 items by manual fiddling" in {
      def savem = for {
        dropped <- FuturifySRC.void(ThingDAO.coll.withWriteConcern(WriteConcern.FSYNCED).drop).toRef
        i <- new RefTraversableOnce((1 to 200).toSeq)
        t1 = Thing(Id(ThingDAO.allocateId), "Number " + i)
        saved <- ThingDAO.saveSafe(t1)
      } yield saved

      val vv = for {
        s <- savem.collect.toFuture
        cursor <- FuturifySRC.apply(ThingDAO.coll.find(bsonDoc()).batchCursor)
        e = EnumerationHelper.enumerateBatchesFromCursor(cursor)
        v <- e.run(Iteratee.fold(0) { case (x, y) => x + y.size })
      } yield v

      vv must beEqualTo(200).await
    }

    "Retrieve data it puts" in  {
      val t1 = Thing(Id(ThingDAO.allocateId), "first")
      (for {
        dropped <- FuturifySRC.void(ThingDAO.coll.withWriteConcern(WriteConcern.FSYNCED).drop)
        saved <- ThingDAO.saveAndFetch(t1).toFuture
      } yield saved.value) must beEqualTo("first").await
    }

    "Retrieve multiple it puts" in  {
      val t1 = Thing(Id(ThingDAO.allocateId), "first")
      val t2 = Thing(Id(ThingDAO.allocateId), "first")

      val feiAll = for {
        dropped <- FuturifySRC.void(ThingDAO.coll.withWriteConcern(WriteConcern.FSYNCED).drop)
        saved1 <- ThingDAO.saveSafe(t1).toFuture
        saved2 <- ThingDAO.saveSafe(t2).toFuture
        all <- ThingDAO.feiFindMany(Filters.eq("value", "first"))
      } yield all

      ThingDAO.refMany(feiAll).collect.map(_.length).toFuture must beEqualTo(2).await
    }

    "Retrieve multiple it puts by id" in  {
      val t1 = Thing(Id(ThingDAO.allocateId), "first")
      val t2 = Thing(Id(ThingDAO.allocateId), "first")

      import Ref._

      val rAll = for {
        dropped <- FuturifySRC.void(ThingDAO.coll.withWriteConcern(WriteConcern.FSYNCED).drop).toRef
        saved1 <- ThingDAO.saveSafe(t1)
        saved2 <- ThingDAO.saveSafe(t2)
        all <- ThingDAO.manyById(Seq(t1.id.id, t2.id.id))
      } yield all

      rAll.collect.toFuture.map(_.length).toFuture must beEqualTo(2).await
    }

    "Retrieve more than 101 items using findMany" in {
      def savem = for {
        dropped <- FuturifySRC.void(ThingDAO.coll.withWriteConcern(WriteConcern.FSYNCED).drop).toRef
        i <- new RefTraversableOnce((1 to 200).toSeq)
        t1 = Thing(Id(ThingDAO.allocateId), "Number " + i)
        saved <- ThingDAO.saveSafe(t1)
      } yield saved

      def findem = ThingDAO.findMany(bsonDoc())

      (for {
        dropped <- FuturifySRC.void(ThingDAO.coll.withWriteConcern(WriteConcern.FSYNCED).drop).toRef
        saved <- savem.collect
        found <- findem.collect
      } yield {
          println(s"Saved ${saved.size}")
          println(s"Found ${found.size}")
          saved.size == found.size
        }).toFuture must beEqualTo(true).await

    }
  }

}
