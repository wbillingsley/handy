package com.wbillingsley.handy.reactivestreams

import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable._
import com.wbillingsley.handy._
import Ref._
import RefMany._
import ProcessorFuncs._
import org.reactivestreams.Publisher

import scala.concurrent.Future

class MapRSpec(implicit ee: ExecutionEnv) extends Specification {

  "MapR" should {

    "convert a short synchronous stream" in {

      val short:RefMany[Int] = (0 until 4).toRefMany
      val pub = new RMPublisher(short)

      val doubled:Publisher[Int] = pub.map(_ * 2)

      doubled.toRefMany.collect.toFuture must be_==(Seq(0, 2, 4, 6)).await

    }

    "convert a short asynchronous stream" in {

      val short:RefMany[Int] = (0 until 4).toRefMany.flatMapOne(x => Future.apply(x).toRef)
      val pub = new RMPublisher(short)

      val doubled:Publisher[Int] = pub.map(_ * 2)

      doubled.toRefMany.collect.toFuture must be_==(Seq(0, 2, 4, 6)).await

    }

  }

}
