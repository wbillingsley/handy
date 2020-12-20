package com.wbillingsley.handy.reactivestreams

import com.wbillingsley.handy.Ref._
import com.wbillingsley.handy.RefMany._
import com.wbillingsley.handy._
import com.wbillingsley.handy.reactivestreams.ProcessorFuncs._
import org.reactivestreams.Publisher
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable._

import scala.concurrent.Future
import scala.concurrent.duration._

class DemandCounterSpec(implicit ee: ExecutionEnv) extends Specification {

    "DemandCounter" should {

        "sequence sends that were already synchronous" in {
            val num = 100000
            val seq = 0 until num
            val dc = new DemandCounter()

            def next(i:Int) = { dc.demand(1); i + 1 }

            val sends:RefMany[Int] = for {
                i <- seq.toRefMany
                r <- new RefFuture(dc.requestToSend())
            } yield next(i)

            dc.demand(1)
            
            sends.collect.map({ case seq =>
              seq.foldLeft((true, -1, 0)) { case ((ordered, x, count), y) => (ordered && y > x, y, count + 1) }                
            }).toFuture must be_===((true, num, seq.length)).awaitFor(5.seconds)
        }

    }

}