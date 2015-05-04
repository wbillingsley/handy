package com.wbillingsley.handy.mongodbasync

import java.util.NoSuchElementException

import com.mongodb.async.{AsyncBatchCursor, SingleResultCallback}
import com.mongodb.async.client.MongoIterable
import org.bson.BsonDocument
import play.api.libs.iteratee._

import scala.concurrent.{ExecutionContext, Promise, Future}


object EnumerationHelper {

  def enumerateBatchesFromCursor(cursor:AsyncBatchCursor[BsonDocument])(implicit ec:ExecutionContext) = Enumerator.fromCallback1[Seq[BsonDocument]](
    retriever = { (first) =>
      def seqs = for {
        list <- FuturifySRC[java.util.List[BsonDocument]](cursor.next)
        seq = scala.collection.JavaConverters.asScalaBufferConverter(list).asScala
      } yield seq

      seqs.map(Some(_)).recover { case n: NoSuchElementException => None }
    },
    onComplete = { () => cursor.close() },
    onError = { (err:String,el:Input[Seq[BsonDocument]]) => cursor.close() }
  )

  /*

  /**
   * Takes an asynchronous cursor, and drives an iteratee with the batches
   * @param cursor
   */
  class EnumerateBatchesFromCursor(cursor:AsyncBatchCursor[BsonDocument])(implicit ec:ExecutionContext) extends Enumerator[Seq[BsonDocument]] {

    override def apply[A](it: Iteratee[Seq[BsonDocument], A]): Future[Iteratee[Seq[BsonDocument], A]] = {
      new RuntimeException("foo").printStackTrace()

      def fit = (for {
        list <- FuturifySRC[java.util.List[BsonDocument]](cursor.next)
        seq = scala.collection.JavaConverters.asScalaBufferConverter(list).asScala
        el = Input.El(seq)
      } yield it.feed(el)) recoverWith { case n: NoSuchElementException => it.feed(Input.EOF) }

      cursor.setBatchSize(7)

      def fEls = for {
        list <- FuturifySRC[java.util.List[BsonDocument]](cursor.next)
        seq = scala.collection.JavaConverters.asScalaBufferConverter(list).asScala
        el = Input.El(seq)
      } yield el

      fEls.flatMap(it.feed(_)) recoverWith { case n: NoSuchElementException => cursor.close(); it.feed(Input.EOF) }
      /*


      it.feed()

      it.fold { (ff) => println("ff was " + ff); ff match {
        case Step.Cont(k) =>
          println("X CALLED")

          val fut = for {
            list <- FuturifySRC[java.util.List[BsonDocument]](cursor.next)
            seq = scala.collection.JavaConverters.asScalaBufferConverter(list).asScala
            el = Input.El(seq)
          } yield {
              println("SEQ LENGHT" + seq.length)
              k.apply(el)
            }

          fut.recoverWith {
            case n: NoSuchElementException => {
              println("X CLOSED")
              Future.successful(k.feed(Input.EOF))
            }
            case t: Throwable => {
              println("FAILED WITH " + t)
              Future.failed(t)
            }
          }

        case s => {
          println(s)
          Future.successful(it)
        }
      }*/
    }

  }
*/
  /**
   * Given a MongoIterable, return something that can drive an Iteratee with batches of results
   * @param mi
   * @return
   */
  def enumerateDocumentBatches(mi:MongoIterable[BsonDocument])(implicit ec:ExecutionContext):Future[Enumerator[Seq[BsonDocument]]] = {
    for {
      cursor <- FuturifySRC[AsyncBatchCursor[BsonDocument]](mi.batchCursor)
    } yield enumerateBatchesFromCursor(cursor)
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
    FuturifySRC[BsonDocument](mi.first)
  }

  def futureSeqDocs(mi:MongoIterable[BsonDocument])(implicit ec:ExecutionContext):Future[Seq[BsonDocument]] = {
    val arr = new java.util.ArrayList[BsonDocument](0)
    for {
      arr <- FuturifySRC[java.util.ArrayList[BsonDocument]](mi.into(arr, _))
    } yield scala.collection.JavaConverters.asScalaBufferConverter(arr).asScala
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

