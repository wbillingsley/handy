package com.wbillingsley.handy.reactivestreams

import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable._
import com.wbillingsley.handy._
import Ref._
import RefMany._
import ProcessorFuncs._
import org.reactivestreams.Publisher

import scala.concurrent.Future
import akka.stream.scaladsl._

class MapRSpec(implicit ee: ExecutionEnv) extends Specification {

  "MapR" should {

    "convert a short synchronous stream" in {

      val short:RefMany[Int] = (0 until 4).toRefMany
      val pub = new RMPublisher(short)

      val doubled:Publisher[Int] = pub.map(_ * 2)

      doubled.toRefMany.collect.toFuture must be_==(Seq(0, 2, 4, 6)).await

    }

    "map a stream of numbers, interoperably with Akka" in new WithAkka {
      val publisher = counterPublisher(0, 100)
      val mapped = new MapR(publisher)({ x => RefSome(x * 2)})

      val mappedSource = Source.fromPublisher(mapped)
      val received = mappedSource.runWith(Sink.seq)

      received.map(_.sum) must be_==((0 until 100).sum * 2).await
    }

  }
  
}
