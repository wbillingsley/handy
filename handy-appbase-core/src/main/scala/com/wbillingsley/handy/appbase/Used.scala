package com.wbillingsley.handy.appbase

import com.wbillingsley.handy.Id

case class Used[T] (target:Id[T,String], time:Long)
