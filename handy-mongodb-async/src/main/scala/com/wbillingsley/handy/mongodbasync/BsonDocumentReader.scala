package com.wbillingsley.handy.mongodbasync

import org.bson.BsonDocument

import scala.collection.mutable
import scala.util.{Success, Try}

trait BsonDocumentReader[T] {
  def read(d:BsonDocument):Try[T]

  def readSeq(s:Seq[BsonDocument]) = {
    val start:Try[mutable.Buffer[T]] = new Success(mutable.Buffer.empty[T])

    s.foldLeft(start) { (tb,doc) =>
      for {
        buf <- tb
        converted <- read(doc)
      } yield {
        buf.append(converted)
        buf
      }
    }
  }
}

trait BsonDocumentWriter[T] {
  def write(t:T):BsonDocument
}

trait BsonDocumentConverter[T] extends BsonDocumentReader[T] with BsonDocumentWriter[T]
