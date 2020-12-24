package com.wbillingsley.handy.reactivestreams

import com.wbillingsley.handy.{Ref, RefFailed, RefFuture, RefIterableOnce, RefItself, RefMany}

import scala.concurrent.Future

object Run {

  def main(args: Array[String]): Unit = {

    val range = 0 until 100000
    var start = System.currentTimeMillis()

    def operation(x: Int, i: Int): Int = {
      if (i % 1000 == 0) {
        printElapsed(i)
      }
      x + i
    }

    def testFuture() = {
      range.foldLeft[Future[Int]](Future.successful(0)) {
        (total, i) => total.flatMap { x => Future.apply(operation(x, i)) }
      }.foreach(_ => printElapsed(range.end))
    }

    def testProcessor() = {
      import RefMany._

      val nums = RefIterableOnce(range).flatMapOne[Int](i => RefFuture(Future.apply(i)))

      // Now double-wrap the nums to force it to use streaming
      val processor = new RMPublisher(nums)
      val refMany = processor.toRefMany

      refMany.foldLeft(0)({
        (total, i) => operation(total, i)
      }).foreach(sum => {
        println(s"Result is $sum")
        printElapsed(range.end)
      })
    }

    def testRefFuture() = {
      range.foldLeft[Ref[Int]](RefItself(0)) {
        (total, i) =>
          total.flatMapOne { x =>
            new RefFuture(Future.apply(operation(x, i)))
          }
      }.recoverWith(
        { case x: Throwable => x.printStackTrace(); RefFailed(x) }
      ).foreach(sum => {
        println(s"Result is $sum")
        printElapsed(range.end)
      })
    }

    //testRefFuture()
    testProcessor()

    def printElapsed(count: Int): Unit = {
      val el = System.currentTimeMillis() - start
      println(s"Count $count Elapsed $el ms. Throughput ${count * 1000.0 / el} ops/s")
    }


  }

}
