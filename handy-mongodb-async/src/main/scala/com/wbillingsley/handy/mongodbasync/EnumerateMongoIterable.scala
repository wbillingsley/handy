package com.wbillingsley.handy.mongodbasync

import java.util
import java.util.NoSuchElementException

import com.mongodb.async.{AsyncBatchCursor, SingleResultCallback}
import com.mongodb.async.client.MongoIterable
import org.bson.BsonDocument
import play.api.libs.iteratee._

import scala.collection.JavaConversions
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Promise, Future}


object EnumerationHelper {

  /**
   * Takes an asynchronous cursor, and drives an iteratee with the batches
   * @param cursor
   */
  class EnumerateBatchesFromCursor(cursor:AsyncBatchCursor[BsonDocument])(implicit ec:ExecutionContext) extends Enumerator[Seq[BsonDocument]] {

    override def apply[A](it: Iteratee[Seq[BsonDocument], A]): Future[Iteratee[Seq[BsonDocument], A]] = {
      it.fold(step => step match {
        case Step.Cont(k) => {
          val pList = Promise[util.List[BsonDocument]]

          // Get the next batch
          cursor.next(new SingleResultCallback[util.List[BsonDocument]] {
            override def onResult(t: util.List[BsonDocument], throwable: Throwable): Unit = {
              if (throwable != null) {
                pList.failure(throwable)
              } else {
                if (t == null) {
                  pList.failure(new NoSuchElementException())
                } else {
                  pList.success(t)
                }
              }
            }
          })

          val fut = for {
            list <- pList.future
            seq = JavaConversions.asScalaBuffer(list)
            el = Input.El(seq)
          } yield el
          fut.map(k.apply(_)).recoverWith {
            case n:NoSuchElementException => Future.successful(k.apply(Input.EOF))
            case t:Throwable => Future.failed(t)
          }
        }
        case _ => Future.successful(it)
      })
    }

  }

  /**
   * Given a MongoIterable, return something that can drive an Iteratee with batches of results
   * @param mi
   * @return
   */
  def enumerateDocumentBatches(mi:MongoIterable[BsonDocument])(implicit ec:ExecutionContext):Future[Enumerator[Seq[BsonDocument]]] = {
    for {
      cursor <- FuturifySRC[AsyncBatchCursor[BsonDocument]](mi.batchCursor(_))
    } yield new EnumerateBatchesFromCursor(cursor)
  }

  /**
   * Given a MongoIterable and a converter, enumerate the found documents in batches
   * @param mi
   * @param converter
   * @tparam T
   * @return
   */
  def enumerateBatches[T](mi:MongoIterable[BsonDocument])(implicit converter:BsonDocumentConverter[T], ec:ExecutionContext):Future[Enumerator[Seq[T]]] = {
    for {
      docEnum <- enumerateDocumentBatches(mi)(ec)
    } yield docEnum through Enumeratee.mapM(batch => Future.fromTry(converter.readSeq(batch)))
  }

  def enumerateIterators[T](mi:MongoIterable[BsonDocument])(implicit converter:BsonDocumentConverter[T], ec:ExecutionContext):Future[Enumerator[Iterator[T]]] = {
    for {
      eb <- enumerateBatches(mi)(converter, ec)
    } yield eb.map(Iterator(_:_*))
  }

  /**
   * Given a MongoIterable and a converter, enumerate the found documents individually
   * @param mi
   * @param converter
   * @tparam T
   * @return
   */
  def enumerate[T](mi:MongoIterable[BsonDocument])(implicit converter:BsonDocumentConverter[T], ec:ExecutionContext):Future[Enumerator[T]] = {
    for {
      seqEnum <- enumerateDocumentBatches(mi)
      docEnum =  seqEnum.flatMap(seq => Enumerator(seq: _*))
    } yield docEnum through Enumeratee.mapM(doc => Future.fromTry(converter.read(doc)))
  }

  /**
   * Given a MongoIterable and a converter, produce a future of the first document
   * @param mi
   * @return
   */
  def futureDoc(mi:MongoIterable[BsonDocument]):Future[BsonDocument] = {
    FuturifySRC[BsonDocument](mi.first(_))
  }

  def futureSeqDocs(mi:MongoIterable[BsonDocument])(implicit ec:ExecutionContext):Future[Seq[BsonDocument]] = {
    for {
      arr <- FuturifySRC[util.ArrayList[BsonDocument]](mi.into(new util.ArrayList[BsonDocument], _))
    } yield JavaConversions.asScalaBuffer(arr)
  }

  def futureSeq[T](mi:MongoIterable[BsonDocument])(implicit converter:BsonDocumentConverter[T], ec:ExecutionContext) = {
    for {
      d <- futureSeqDocs(mi)
      converted <- Future.fromTry(converter.readSeq(d))
    } yield converted
  }

  /**
   * Given a MongoIterable and a converter, produce a future of the first document
   * @param mi
   * @param converter
   * @tparam T
   * @return
   */
  def futureOne[T](mi:MongoIterable[BsonDocument])(implicit converter:BsonDocumentConverter[T], ec:ExecutionContext) = {
    for {
      d <- futureDoc(mi)
      converted <- Future.fromTry(converter.read(d))
    } yield converted
  }
}

