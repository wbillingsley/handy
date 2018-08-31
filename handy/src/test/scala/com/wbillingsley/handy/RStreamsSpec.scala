package com.wbillingsley.handy

import com.wbillingsley.handy.reactivestreams._
import org.reactivestreams.Publisher
import org.specs2.mutable.Specification

import Ref._
import RefMany._
import ProcessorFuncs._

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

object RStreamsSpec extends Specification {

  "DemandCounter" should {

    "sequence sends" in {
      val seq = Seq(1,2,3,4)
      val dc = new DemandCounter()

      def next(i:Int) = { dc.demand(1); i + 1 }

      val sends:RefMany[Int] = for {
        i <- seq.toRefMany
        r <- new RefFuture(dc.requestToSend())
      } yield next(i)

      dc.demand(1)

      sends.collect.toFuture must be_==(Seq(2, 3, 4, 5)).await
    }

  }

  "RMPublisher" should {

    "Stream a Seq into a Seq without corrupting it" in {

      val seq = Seq(1, 2, 3, 4)
      val rm = seq.toRefMany

      val pub = new RMPublisher(rm)

      val h = ProcessorFuncs.collect(pub)
      h.toFuture must be_==(Seq(1, 2, 3, 4)).await

    }

    "Map a stream correctly" in {
      val seq = Seq(1, 2, 3, 4)
      val rm = seq.toRefMany

      val pub = new RMPublisher(rm)
      val mapr = new MapR(pub)({ x:Int => (x + 1).itself })
      val h = ProcessorFuncs.collect(mapr)
      h.toFuture must be_==(Seq(2, 3, 4, 5)).await

    }

    "Fold a stream correctly" in {
      val seq = Seq(1, 2, 3, 4)
      val rm = seq.toRefMany

      val pub = new RMPublisher(rm)
      val foldr = new FoldProcessor[Int, Int](pub)(0)({ case (x, y)  => x + y })
      foldr.toFuture must be_==(10).await
    }

    "Concatenate publishers correctly" in {
      val seq = Seq(1, 2, 3, 4)
      val rm = seq.toRefMany
      def pub = new RMPublisher(rm)

      val concatR = new ConcatRM(Seq(pub, pub, pub).toRefMany)

      ProcessorFuncs.collect(concatR).toFuture must be_==(seq ++ seq ++ seq).await
    }


    "Concatenate synchronously produced asynchronous publishers correctly" in {
      val ass = new AsyncStreamer

      def stream(x:Int) = new RMPublisher(ass.getX(x))
      val a = stream(5)
      val b = stream(5)
      val c = stream(5)
      val rm = Seq(a, b, c).toRefMany
      val pp:Publisher[Publisher[Int]] = new RMPublisher(rm)

      val cc = new ConcatProcessor[Int](pp)
      ass.completeThePromises()

      ProcessorFuncs.collect(cc).toFuture must be_==(0 until 15).await
    }

  }

  "RefPublisher" should {

    "map and collect correctly" in {
      val seq = Seq(1, 2, 3, 4)
      val rm = seq.toRefMany
      val pub = new RMPublisher(rm)
      val refPub = new RefPublisher(pub)

      (for {
        i <- refPub
      } yield i * 2).collect.toFuture must be_==(Seq(2, 4, 6, 8)).await
    }

    "flatmap and collect correctly" in {
      val seq = Seq(1, 2, 3, 4)
      def rm = seq.toRefMany
      def pub = new RMPublisher(rm)
      def refPub = new RefPublisher(pub)

      (
        for { i <- refPub; j <- refPub } yield i * j
      ).collect.toFuture must be_==(
        for { i <- seq; j <- seq} yield i * j
      ).await
    }

    "map async" in {
      val ass = new AsyncStreamer

      // A stream of 100 asynchronus numbers, each mapped to add another asynchronus number
      def stream(x:Int) = new RMPublisher(ass.getX(x))
      val mapped = stream(100).map(_ * 2)
      ass.completeThePromises()

      ProcessorFuncs.collect(mapped).toFuture must be_==((0 until 100).map(_ * 2)).await
    }


    "concat async 2" in {
      val ass = new AsyncStreamer

      // A stream of 100 asynchronus numbers, each mapped to add another asynchronus number
      def stream(x:Int) = new RMPublisher(ass.getX(x))
      def streamSync(x:Int) = new RMPublisher(new RefTraversableOnce(0 until x))

      val mapped:Publisher[Publisher[Int]] = streamSync(3).map({ _ => println("generating a stream"); stream(10)})
      val cc = new ConcatProcessor[Int](mapped)
      ass.completeThePromises()

      ProcessorFuncs.collect(cc).toFuture must be_==((3 until 33)).await
    }


    "concat async" in {
      val ass = new AsyncStreamer

      // A stream of 100 asynchronus numbers, each mapped to add another asynchronus number
      def stream(x:Int) = new RMPublisher(ass.getX(x))

      val mapped = stream(2).map(_ => stream(10)).toRefMany
      for {
        m <- mapped
      } println("POO" + m)
      val cc = new ConcatRM[Int](mapped)
      ass.completeThePromises()

      ProcessorFuncs.collect(cc).toFuture must be_==((3 until 33)).await
    }


    "flatMap across genuinely asynchonrous streams correctly" in {

      val ass = new AsyncStreamer

      // A stream of 100 asynchronus numbers, each mapped to add another asynchronus number
      // def stream(x:Int) = new RMPublisher(getX(x))
       def stream(x:Int) = new MapR[Int, Int](new RMPublisher(ass.getX(x)))({ case i => (1 + i).itself })

      // Now, if in our test we sum two streams, we should get 0..99 + 100..199 + 200..299 + 300..399 but the order
      // is unknown

      val stream1 = stream(10).toRefMany
      val stream2 = stream(10).toRefMany

      val result = for {
        i1 <- stream1
        i2 <- stream2
      } yield {
        println(s"i1 $i1 i2 $i2")
        i1 + i2
      }

      ass.completeThePromises()

      result.fold(0)(_ + _).toFuture must be_==((0 until 400).sum).await
    }

  }

}

/**
  * A cheaty little utility class.
  *
  * It produces asynchronous RefMany by producing promises and storing them. You then call your asynchronous function
  * (producing an incomplete Future). And then call `completeThePromises` to make it all happen.
  */
class AsyncStreamer {

  import scala.concurrent.ExecutionContext.Implicits.global

  val promises:mutable.Buffer[Promise[Int]] = mutable.Buffer.empty

  // Get a future Int and put the promise in the list of uncompleted promises
  def asyncFunc():Future[Int] = synchronized {
    val p = Promise.apply[Int]
    promises.append(p)
    p.future
  }


  // At the end, we're just going to complete each promise with a successive number
  def completeThePromises() = {
    for {
      (p, i) <- promises.zipWithIndex
    } p.success(i)
    println(s"Completed ${promises.length} promises")
  }

  // A reference to one promise
  def getOne():Ref[Int] = asyncFunc().toRef

  // A reference to many promises
  def getX(x:Int):RefMany[Int] = {
    println(s"asked for $x numbers")
    val sf = for (i <- 0 until x) yield {
      asyncFunc()
    }
    sf.toRefMany.flatMap(_.toRef)
  }
}
