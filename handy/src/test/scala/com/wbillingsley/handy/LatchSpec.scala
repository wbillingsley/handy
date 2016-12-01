package com.wbillingsley.handy;

import org.specs2.mutable._
import scala.concurrent.ExecutionContext.Implicits.global
import Ref._

import scala.concurrent.Promise

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


  }

}
