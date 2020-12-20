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

trait WithAkka extends After {
    implicit val system = ActorSystem("QuickStart")
    implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system).withInputBuffer(
      initialSize = 16,
      maxSize = 16))

    def counter(start:Int, end:Int) = Source(start until end).throttle(1, 1.millisecond)

    def counterPublisher(start:Int, end:Int) = counter(start, end).runWith(Sink.asPublisher(fanout = true))

    def after = system.terminate()
}