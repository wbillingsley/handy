package com.wbillingsley.handy;

import org.specs2.mutable._
import scala.concurrent.ExecutionContext.Implicits.global
import Ref._

import scala.concurrent.{Future, Promise}

object LatchSpec extends Specification {

  "A latch" should {

    "produce its value when required" in {
      val p = Promise[Int]
      val l = Latch.lazily(p.future)
      p.success(1)
      l.request should be_==(1).await
    }

    "clear itself if its parent clears" in {
      val p = Promise[Int]
      val l = Latch.lazily(p.future)
      val dependentLatch = l.map(_ * 2)

      // Complete the first latch
      p.success(1)

      // Clear the first latch; this should synchronously clear its dependent latches
      l.clear()

      dependentLatch.isCompleted should be_==(false)
    }

    "clear itself if its parent changes" in {
      val p = Promise[Int]
      val l = Latch.lazily(p.future)
      val dependentLatch = l.map(_ * 2)

      // Complete the first latch
      p.success(1)

      // Clear the first latch; this should synchronously clear its dependent latches
      l.fill(4)

      dependentLatch.isCompleted should be_==(false)
    }


    "recompute itself on demand if its parent changes" in {
      val p = Promise[Int]
      val l = Latch.lazily(p.future)
      val dependentLatch = l.map(_ * 2)

      // Complete the first latch
      p.success(1)

      // Clear the first latch; this should synchronously clear its dependent latches
      l.fill(4)

      dependentLatch.request should be_==(8).await
    }

    "support for comprehensions that produce a latch" in {

      val latch = Latch.immediate(1)

      // A dummy asynchronous function
      def asyncFunc(i:Int) = Future.successful(i * 2)

      val dependent:Latch[Int] = for {
        a <- latch
        b <- asyncFunc(a)
        c <- asyncFunc(b)
      } yield c

      latch.fill(2)

      dependent.request should be_==(8).await
    }

    "support combinations of latches" in {

      val latchA = Latch.immediate(1)
      val latchB = Latch.immediate(10)

      // A dummy asynchronous function
      def asyncFunc(a:Int, b:Int) = Future.successful(a * b)

      val dependent:Latch[Int] = latchA.combine(latchB)(_ * _)
      latchB.fill(8)

      dependent.request should be_==(8).await
    }


  }

}
