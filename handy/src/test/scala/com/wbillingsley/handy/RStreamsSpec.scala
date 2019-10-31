package com.wbillingsley.handy

import com.wbillingsley.handy.reactivestreams._
import org.reactivestreams.Publisher
import org.specs2.mutable.Specification
import Ref._
import RefMany._
import ProcessorFuncs._
import org.specs2.concurrent.ExecutionEnv

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._

class RStreamsSpec(implicit ee: ExecutionEnv) extends Specification {

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
      val mapr = new MapR(pub)({ x:Int => RefSome(x + 1) })
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

    "concatenate RefMany publishers" in {
      val i = 50
      val j = 50

      def stream(x:Int) = new RMPublisher({
        val nums = RefIterableOnce(0 until x)
        nums.flatMapOne[Int](i => RefFuture(Future.apply(i)))
      })

      val mapped = stream(i).map(_ => stream(j)).toRefMany
      val cc = new ConcatRM[Int](mapped)

      ProcessorFuncs.collect(cc).map(_.length).toFuture must be_==(i * j).await
    }

  }

  "AsyncStreamer" should {
    "provide the numbers 0 to 100" in {
      val streamer = new AsyncStreamer

      val many = streamer.getX(1000)

      val sum = many.foldLeft(0)( _ + _).toFuture

      streamer.completeThePromises()

      sum must be_==((0 until 1000).sum).await

    }
  }

}

/**
  * A cheaty little utility class.
  *
  * It produces asynchronous RefMany by producing promises and storing them.
  * You then call your asynchronous function
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
  def completeThePromises():Int = {
    for {
      (p, i) <- promises.zipWithIndex
    } p.success(i)
    promises.length
  }

  // A reference to one promise
  def getOne():Ref[Int] = asyncFunc().toRef

  // A reference to many promises
  def getX(x:Int):RefMany[Int] = {
    val sf = for (i <- 0 until x) yield {
      asyncFunc()
    }
    sf.toRefMany.flatMap(_.toRef)
  }
}
