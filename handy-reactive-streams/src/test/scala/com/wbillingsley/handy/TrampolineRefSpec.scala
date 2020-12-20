package com.wbillingsley.handy

import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class TrampolineRefSpec(implicit ee: ExecutionEnv) extends Specification {

  "Trampolines.tramplineR" should {

    import Trampolines._

    "evaluate a DoneR immediately" in {
      Done(RefItself(5)).result must be_==(RefItself(5))
    }

    "evaluate a long sequence of ContinueRs" in {

      var count = 150000
      def countDown():Trampoline[Int] = {
        count = count - 1
        if (count > 0) {
          Continue(countDown)
        } else Done(count)
      }

      Continue(countDown).result must be_==(0)
    }

  }

}
