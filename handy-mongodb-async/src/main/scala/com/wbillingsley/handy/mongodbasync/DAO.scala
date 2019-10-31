package com.wbillingsley.handy.mongodbasync

import com.mongodb.WriteConcern
import com.mongodb.client.model._
import com.mongodb.client.result.UpdateResult
import com.wbillingsley.handy._
import com.wbillingsley.handyplay.RefEnumIter
import org.bson.{BsonObjectId, BsonArray, BsonDocument}
import org.bson.conversions.Bson
import org.bson.types.ObjectId
import play.api.libs.iteratee.Enumerator
import scala.collection.JavaConverters._

import scala.concurrent.{ExecutionContext, Future}

/**
 *
 * @param db A database connector, holding properties such as the port, username, password
 * @param clazz A reference to the class object for the type this retrieves.
 * @param collName The name of the collection in the database
 * @param executionContext The execution context (ie, thread pool) that futures should be created on.
 * @param converter Converts from BSON to the transfer object
 * @tparam DataT The type this DAO retrieves
 */
class DAO[DataT <: HasStringId[DataT]] (
  db: DB,
  clazz:Class[DataT],
  collName:String,
  writeConcern: WriteConcern = WriteConcern.FSYNCED
) (
  implicit val executionContext:ExecutionContext,
  converter: BsonDocumentConverter[DataT]
) {

  implicit object LookUp extends LookUp[DataT, String] {

    def one[K <: String](r: Id[DataT, K]) = byId(r.id)

    def many[K <: String](r: Ids[DataT, K]) = manyById(r.ids)

  }

  def lookUp = LookUp

  /**
   * The collection in the database
   */
  def coll = db.coll(collName).withWriteConcern(writeConcern)

  /**
   * Allocates a new ID
   */
  def allocateId = new ObjectId().toHexString

  def refMany[T](fei:Future[Enumerator[Iterator[T]]]) = {
    new RefFuture(fei).flatMapMany(new RefEnumIter(_))
  }

  def feiFindMany(query:Bson):Future[Enumerator[Iterator[DataT]]] = {
    EnumerationHelper.enumerateIterators(coll.find(query))
  }

  def findMany(query:Bson):RefMany[DataT] = refMany(feiFindMany(query))

  def feiFindSorted(query:Bson, sort:Bson):Future[Enumerator[Iterator[DataT]]] = {
    EnumerationHelper.enumerateIterators(coll.find(query).sort(sort))
  }

  def findSorted(query:Bson, sort: Bson) = refMany(feiFindSorted(query, sort))

  def fFindOne(query:Bson):Future[DataT] = {
    EnumerationHelper.futureOne(coll.find(query))
  }

  def findOne(query:Bson) = new RefFuture(fFindOne(query))


  /**
   * A test on whether _id matches
   */
  def idIs(id:Id[_,String]) = Filters.eq("_id", new ObjectId(id.id))

  /**
   * A test on whether _id is in the set.
   */
  def idsIn(ids:Ids[_, String]) = {
    val arr = for {
      id <- ids.ids
    } yield new BsonObjectId(new ObjectId(id))
    Filters.in("_id", new BsonArray(arr.asJava))
  }

  /**
   * Fetches and deserializes items by their ID
   */
  def byId(id:String) = {
    findOne(idIs(Id(id)))
  }

  /**
   * Fetches and deserializes items by their ID
   */
  def manyById(ids:Seq[String]):RefMany[DataT] = {
    /*
     * First, fetch the items.  These might not return in the same order as the
     * sequence of IDs, and duplicate IDs will only return once
     */
    val futureSeq = EnumerationHelper.futureSeq(coll.find(idsIn(Ids(ids))))

    /*
     * As the order of the items is unspecified, build a map from id -> item
     */
    val futIdMap = for {
      seq <- futureSeq
    } yield {
      val pairs = for (item <- seq) yield item.id.id -> item
      val map = Map(pairs.toSeq:_*)
      map
    }

    /*
     * For each id in the requested sequence, return the corresponding item in the map.
     */
    val reordered = for (
      map <- new RefFuture(futIdMap);
      id <- new RefIterableOnce(ids);
      item <- Ref(map.get(id))
    ) yield item

    reordered
  }

  def updateOneSafe(query:Bson, update:Bson, upsert:Boolean = false) = {
    val futurified = new FuturifySRC[UpdateResult]
    coll.updateOne(query, update, new UpdateOptions().upsert(upsert), futurified.callback)
    futurified.future
  }

  def updateManySafe(query:Bson, update:Bson, upsert:Boolean = false) = {
    val futurified = new FuturifySRC[UpdateResult]
    coll.updateMany(query, update, new UpdateOptions().upsert(upsert), futurified.callback)
    futurified.future
  }

  def updateAndFetch(query:Bson, update:Bson, upsert:Boolean = false):Ref[DataT] = {
    for {
      ur <- new RefFuture(updateOneSafe(query, update, upsert))
      fetched <- findOne(query)
    } yield fetched
  }

  def updateSafe(query:Bson, update:Bson, item:DataT, upsert:Boolean = false):Ref[DataT] = {
    for {
      ur <- new RefFuture(updateOneSafe(query, update, upsert))
    } yield item
  }

  def saveSafe(item:DataT) = {
    new RefFuture(findAndReplace(idIs(item.id), item, upsert=true))
  }

  def saveAndFetch(item:DataT) = {
    for {
      i <- saveSafe(item)
      fetched <- byId(item.id.id)
    } yield fetched
  }


  def findAndReplace(query:Bson, item:DataT, upsert:Boolean=false) = {
    FuturifySRC[BsonDocument](coll.findOneAndReplace(
      query,
      converter.write(item),
      new FindOneAndReplaceOptions().returnDocument(ReturnDocument.AFTER).upsert(upsert),
      _
    )) flatMap {x => Future.fromTry(converter.read(x)) }
  }
}
