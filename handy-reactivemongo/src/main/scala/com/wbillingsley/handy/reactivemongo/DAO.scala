package com.wbillingsley.handy.reactivemongo

import reactivemongo.api._
import reactivemongo.bson._
import reactivemongo.core.commands.GetLastError

import com.wbillingsley.handy._
import com.wbillingsley.handyplay._
import Ref._
import Id._
import Ids._
import scala.concurrent.ExecutionContext

trait DAO {

  type DataT <: HasStringId[DataT]

  import RefFuture.executionContext
  
  implicit object LookUp extends LookUp[DataT, String] {
    
    def one[K <: String](r:Id[DataT, K]) = byId(r.id)
    
    def many[K <: String](r:Ids[DataT, K]) = manyById(r.ids)
    
  }

  def lookUp = LookUp

  /**
   * A reference to the class object for the type this retrieves.
   * Used because the generic type is erased, but at runtime we need to know the
   * class in lookupPf.  (We would use TypeTags except we're waiting for the
   * thread safety issue with them to be fixed.)
   */
  val clazz:Class[DataT]
  
  /**
   * The name of the collection in the database
   */
  val collName: String
  
  /**
   * A database connector, holding properties such as the port, username, password
   */
  val db:DBConnector
  
  /**
   * The collection in the database
   */
  def coll = db.coll(collName)
  
  /**
   * Converts from BSON to the transfer object
   */
  implicit val bsonReader:BSONDocumentReader[DataT];

  /**
   * The execution context (ie, thread pool) that futures should be created on.
   */
  implicit def executionContext:ExecutionContext;

  /**
   * A test on whether _id matches. Depending on db.useBSONObjectIds, this
   * will either expect the id in the database to be a BSONObjectID or a BSONString 
   */
  def idIs[T](id:Id[T, String]):(String, BSONValue) = {
    if (db.useBSONIds) {
      "_id" -> BSONObjectID(id.id)
    } else {
      "_id" -> BSONString(id.id)
    }
  }
  
  /**
   * A test on whether _id is in the set. Depending on db.useBSONObjectIds, this
   * will either expect the id in the database to be a BSONObjectID or a BSONString 
   */
  def idsIn[T](ids:Ids[T, String]):(String, BSONValue) = {
    if (db.useBSONIds) {
      val oids = for (id <- ids.ids) yield BSONObjectID(id)
      
      "_id" -> BSONDocument("$in" -> oids.toSet)
    } else {
      "_id" -> BSONDocument("$in" -> ids.ids.toSet)
    }
  }
  
  /**
   * Fetches and deserializes items by their ID
   */
  def byId(id:String) = {
    findOne(BSONDocument(idIs(id.asId[DataT])))
  } 
  
  /**
   * Fetches and deserializes items by their ID
   */
  def manyById(ids:Seq[String]) = {
    /*
     * First, fetch the items.  These might not return in the same order as the
     * sequence of IDs, and duplicate IDs will only return once
     */
    val rMany = findMany(BSONDocument(idsIn(ids.asIds[DataT])))
    
    /*
     * As the order of the items is unspecified, build a map from id -> item
     */
    val rMap = for (trav <- rMany.toRefOne) yield {
      val pairs = for (item <- trav) yield item.id.id -> item
      val map = Map(pairs.toSeq:_*)
      map
    }
    
    /*
     * For each id in the requested sequence, return the corresponding item in the map.
     */
    for (
        map <- rMap; 
        id <- ids.toRefMany;
        item <- Ref(map.get(id))
    ) yield item
  }
  
  /**
   * Allocates a new ID
   */
  def allocateId = BSONObjectID.generate.stringify
  
  def updateAndFetch(query:BSONDocument, update:BSONDocument, upsert:Boolean = false):Ref[DataT] = {
    val c = coll
    val fle = c.update(query, update, GetLastError(true), upsert=upsert) 
    val fut = fle.map { _ => new RefFutureOption(c.find(query).one[DataT]) } recover { case x:Throwable => RefFailed(x) }
    new RefFutureRef(fut)
  }
  
  def updateSafe(query:BSONDocument, update:BSONDocument, item:DataT, upsert:Boolean = false):Ref[DataT] = {
    val c = coll
    val fle = c.update(query, update, GetLastError(true), upsert=upsert) 
    val fut = fle.map { _ => RefItself(item) } recover { case x:Throwable => RefFailed(x) }
    new RefFutureRef(fut)
  }

  def updateUnsafe(query:BSONDocument, update:BSONDocument, item:DataT, upsert:Boolean = false):Ref[DataT] = {
    val c = coll
    val fle = c.update(query, update, GetLastError(false), upsert=upsert) 
    val fut = fle.map { _ => RefItself(item) } recover { case x:Throwable => RefFailed(x) }
    new RefFutureRef(fut)
  }
  
  def saveSafe(doc:BSONDocument, item:DataT):Ref[DataT] = {
    val c = coll
    val fle = c.save(doc, GetLastError(true)) 
    val fut = fle.map { _ => RefItself(item) } recover { case x:Throwable => RefFailed(x) }
    new RefFutureRef(fut)    
  }

  def findMany(query:BSONDocument):RefMany[DataT] = {
    try {
      new RefEnumIter(coll.find(query).cursor[DataT].enumerateBulks(maxDocs=Int.MaxValue, stopOnError=true))
    } catch {
      case (x:Throwable) => new RefFailed(x)
    }
  }
  def findMany(query:Ref[BSONDocument]):RefMany[DataT] = query flatMap findMany

  def findSorted(query:BSONDocument, sort:BSONDocument):RefMany[DataT] = {
    try {
      new RefEnumIter(coll.find(query).sort(sort).cursor[DataT].enumerateBulks(maxDocs=Int.MaxValue, stopOnError=true))
    } catch {
      case (x:Throwable) => new RefFailed(x)
    }
  }
  def findSorted(query:Ref[BSONDocument], sort:Ref[BSONDocument]):RefMany[DataT] = {
    query flatMap { q =>
      sort flatMap { s =>
        findSorted(q,s)
      }
    }
  }

  
  def findOne(query:BSONDocument):Ref[DataT] = {
    try {
      new RefFutureOption(coll.find(query).one[DataT])
    } catch {
      case (x:Throwable) => new RefFailed(x)
    }
  }
  def findOne(query:Ref[BSONDocument]):Ref[DataT] = query flatMap findOne

}