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

class RMPublisherSpec(implicit ee: ExecutionEnv) extends Specification {

    "RMPublisher" should {

        "publish a simple sequence" in new WithAkka {
            // Start with a simple RefMany
            val range = (0 until 100)
            val rm = new RefTraversableOnce(range)

            // publish it
            val publisher = new RMPublisher(rm)

            // use an Akka Sink to receive the stream
            val source = Source.fromPublisher(publisher)
            val received = source.runWith(Sink.seq)

            // Check the numbers are all still in-order and there's the right number of them
            val check = received.map(_.foldLeft((true, -1, 0)) { case ((ordered, x, count), y) => 
              (ordered && y > x, y, count + 1)
            })
            check must be_==((true, 99, 100)).await
        }

        "publish a RefMany of already-completed Futures" in new WithAkka {
            // Create a Publisher of completed Futures
            def stream(x:Int) = new RMPublisher({
                val nums = RefTraversableOnce(0 until x)
                nums.flatMapOne[Int](i => RefFuture(Future.apply(i)))
            })

            // use an Akka Sink to receive the stream
            val source = Source.fromPublisher(stream(10))
            val received = source.runWith(Sink.seq)

            // Check the numbers are all still in-order and there's the right number of them
            val check = received.map(_.foldLeft((true, -1, 0)) { case ((ordered, x, count), y) => 
              (ordered && y > x, y, count + 1)
            })
            check must be_==((true, 9, 10)).await
        }      

        "flatMap across asynchronous streams correctly" in new WithAkka {
            val result = for {
                i1 <- counterPublisher(0, 10).toRefMany
                i2 <- counterPublisher(10, 20).toRefMany
            } yield {
                i1 + i2
            }

            result.foldLeft(0)(_ + _).toFuture must be_==((for { i <- 0 until 10; j <- 10 until 20} yield i+j).sum).awaitFor(10.seconds)
        }                

        "flatMap across asynchronous streams correctly" in new WithAkka {

            def restream(x:Int) = counterPublisher(0, x).toRefMany.map(_ + 1)

            val stream1 = restream(2) 
            val stream2 = restream(2) 

            val result = for {
                i1 <- stream1
                i2 <- stream2
            } yield {
                i1 + i2
            }

            result.foldLeft(0)(_ + _).toFuture must be_==(5).awaitFor(1.seconds)
        }        

    }

}