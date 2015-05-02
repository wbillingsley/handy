package com.wbillingsley.handy.appbase

import com.wbillingsley.handy._

case class Registration[T, R, P <: HasKind](

  id: Id[Registration[T, R, P], String],

  user: Id[User, String],

  target: Id[T, String],

  roles: Set[R] = Set.empty,

  provenance: P,

  updated:Long = System.currentTimeMillis,

  created:Long = System.currentTimeMillis

) extends HasStringId[Registration[T, R, P]]

