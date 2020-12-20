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

        "flatten a Publisher of Publishers that has been produced by MapR" in new WithAkka {

            // Create a Publisher of completed Futures
            def stream(x:Int) = new RMPublisher({
                val nums = RefIterableOnce(0 until x)
                nums.flatMapOne[Int](i => RefFuture(Future.successful(i)))
            })

            // Create a Publisher of a RefMany that is backed by a Range.
            def streamSync(x:Int) = new RMPublisher(RefIterableOnce(0 until x))

            val mapped:Publisher[Publisher[Int]] = streamSync(3).map({ _ => stream(10) })
            val cc = new ConcatProcessor[Int](mapped)

            val mappedSource = Source.fromPublisher(cc)
            val received = mappedSource.runWith(Sink.seq)

            received.map(_.sum) must be_==((0 until 10).sum * 3).await
        }
            
    }

}