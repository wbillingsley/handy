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

class ConcatProcessorSpec(implicit ee: ExecutionEnv) extends Specification {

    "Our Akka test setup" should {
        "concatenate a publisher of publishers" in new WithAkka {

            def listOfPublishers = for {
                i <- 0 until 10
            } yield counterPublisher(i * 10, (i+1) * 10)

            val publisherOfPublishers = Source[Publisher[Int]](listOfPublishers).runWith(Sink.asPublisher(fanout = true))

            val sourceOfPublishers = Source.fromPublisher(publisherOfPublishers)
            val concatenated = sourceOfPublishers.flatMapConcat { x => Source.fromPublisher(x) }

            //val concatenated = sourceOfSources.fold(Source(List.empty[Int])) { case (x,y) => x.concat(y) }

            concatenated.runWith(Sink.seq) must be_==((0 until 100).toSeq).await

        }
    }


    "ConcatProcessor" should {
        "concatenate a publisher of publishers" in new WithAkka {

            def listOfPublishers = for {
                i <- 0 until 10
            } yield counterPublisher(i * 10, (i+1) * 10)

            val publisherOfPublishers = Source[Publisher[Int]](listOfPublishers).runWith(Sink.asPublisher(fanout = true))

            val concat = new ConcatProcessor(publisherOfPublishers)

            val concatenated = Source.fromPublisher(concat)
            val result = concatenated.runWith(Sink.seq).map(_.toSeq)

            result must be_==((0 until 100).toSeq).await

        }
    }

}