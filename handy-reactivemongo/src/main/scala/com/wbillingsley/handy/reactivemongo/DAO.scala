package com.wbillingsley.handy.reactivemongo

import reactivemongo.api._
import reactivemongo.bson._
import reactivemongo.core.commands.GetLastError

import com.wbillingsley.handy._
import com.wbillingsley.handyplay._
import Ref._
import play.api.libs.concurrent.Execution.Implicits._

trait DAO[T <: HasStringId] {
  
  /**
   * Partial function for looking up RefById to this class
   */
  val lookupPf: PartialFunction[RefById[Any, Any], Ref[T]] = { 
    case r if r.clazz isAssignableFrom(clazz) => {
      byId(r.id.toString)
    }
  }

  /**
   * Partial function for looking up RefManyById to this class
   */
  val lookupManyPf: PartialFunction[RefManyById[Any, Any], RefMany[T]] = { 
    case r if r.clazz isAssignableFrom(clazz) => {
      manyById(r.rawIds.map(_.toString))
    }
  }
      
  /**
   * A reference to the class object for the type this retrieves.
   * Used because the generic type is erased, but at runtime we need to know the
   * class in lookupPf.  (We would use TypeTags except we're waiting for the
   * thread safety issue with them to be fixed.)
   */
  val clazz:Class[T]
  
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
  implicit val bsonReader:BSONDocumentReader[T];
  
  /**
   * Implicit conversion that allows Ref[_] to be written as BSON
   */
  implicit def RefWriter[T <: HasStringId] = new BSONWriter[Ref[T], BSONValue] {
    def write(r:Ref[T]) = {
      if (db.useBSONIds) {
        r.getId.map(new BSONObjectID(_)).getOrElse(BSONNull)
      } else {
        r.getId.map(BSONString(_)).getOrElse(BSONNull)
      }
    }
  }

  /**
   * A test on whether _id matches. Depending on db.useBSONObjectIds, this
   * will either expect the id in the database to be a BSONObjectID or a BSONString 
   */
  def idIs(id:String):(String, BSONValue) = {
    if (db.useBSONIds) {
      "_id" -> new BSONObjectID(id)
    } else {
      "_id" -> BSONString(id) 
    }
  }
  
  /**
   * A test on whether _id is in the set. Depending on db.useBSONObjectIds, this
   * will either expect the id in the database to be a BSONObjectID or a BSONString 
   */
  def idsIn(ids:Seq[String]):(String, BSONValue) = {
    if (db.useBSONIds) {
      val oids = for (id <- ids) yield new BSONObjectID(id)
      
      "_id" -> BSONDocument("$in" -> oids.toSet)
    } else {
      "_id" -> BSONDocument("$in" -> ids.toSet) 
    }
  }
  
  /**
   * Fetches and deserializes items by their ID
   */
  def byId(id:String) = {
    val fo = coll.find(BSONDocument(idIs(id))).one[T]    
    new RefFutureOption(fo)
  } 
  
  /**
   * Fetches and deserializes items by their ID
   */
  def manyById(ids:Seq[String]) = {
    /*
     * First, fetch the items.  These might not return in the same order as the
     * sequence of IDs, and duplicate IDs will only return once
     */
    val rMany = findMany(BSONDocument(idsIn(ids)))
    
    /*
     * As the order of the items is unspecified, build a map from id -> item
     */
    val rMap = for (trav <- rMany.toRefOne) yield {
      val pairs = for (item <- trav) yield item.id -> item
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
  
  /**
   * Creates a blank unsaved object
   */
  def unsaved:T
  
  def updateAndFetch(query:BSONDocument, update:BSONDocument, upsert:Boolean = false):Ref[T] = {
    val c = coll
    val fle = c.update(query, update, GetLastError(true), upsert=upsert) 
    val fut = fle.map { _ => new RefFutureOption(c.find(query).one[T]) } recover { case x:Throwable => RefFailed(x) }
    new RefFutureRef(fut)
  }
  
  def updateSafe(query:BSONDocument, update:BSONDocument, item:T, upsert:Boolean = false):Ref[T] = {
    val c = coll
    val fle = c.update(query, update, GetLastError(true), upsert=upsert) 
    val fut = fle.map { _ => RefItself(item) } recover { case x:Throwable => RefFailed(x) }
    new RefFutureRef(fut)
  }

  def updateUnsafe(query:BSONDocument, update:BSONDocument, item:T, upsert:Boolean = false):Ref[T] = {
    val c = coll
    val fle = c.update(query, update, GetLastError(false), upsert=upsert) 
    val fut = fle.map { _ => RefItself(item) } recover { case x:Throwable => RefFailed(x) }
    new RefFutureRef(fut)
  }
  
  def saveSafe(doc:BSONDocument, item:T):Ref[T] = {
    val c = coll
    val fle = c.save(doc, GetLastError(true)) 
    val fut = fle.map { _ => RefItself(item) } recover { case x:Throwable => RefFailed(x) }
    new RefFutureRef(fut)    
  }
  
  def findMany(query:BSONDocument):RefMany[T] = {
    new RefEnumIter(coll.find(query).cursor[T].enumerateBulks)
  }
  
  def findOne(query:BSONDocument):Ref[T] = {
    new RefFutureOption(coll.find(query).one[T])    
  }
}