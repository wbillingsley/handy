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

trait WithAkka extends After {
    implicit val system = ActorSystem("QuickStart")
    implicit val materializer = ActorMaterializer()

    def counter(start:Int, end:Int) = Source(start until end).throttle(1, 1.millisecond)

    def counterPublisher(start:Int, end:Int) = counter(start, end).runWith(Sink.asPublisher(fanout = true))

    def after = system.terminate()
}