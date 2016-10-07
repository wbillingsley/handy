package com.wbillingsley.handy

import com.wbillingsley.handy.reactivestreams._
import org.specs2.mutable.Specification

import Ref._

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
      val rm = seq.toRefMany
      val pub = new RMPublisher(rm)
      val refPub = new RefPublisher(pub)

      (
        for { i <- refPub; j <- refPub } yield i * j
      ).collect.toFuture must be_==(
        for { i <- seq; j <- seq} yield i * j
      ).await
    }
  }


}
