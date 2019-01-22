package com.wbillingsley.handy.reactivestreams

import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable._
import com.wbillingsley.handy._
import Ref._
import RefMany._
import ProcessorFuncs._
import org.reactivestreams.Publisher

import scala.concurrent.Future

import akka._
import akka.stream._
import akka.stream.scaladsl._
import akka.actor.ActorSystem
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
            }).toFuture must be_===((true, num, seq.length)).await
        }

    }

}