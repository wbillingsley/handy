package com.wbillingsley.handy.appbase

import com.wbillingsley.handy.{HasStringId, Id}

/**
 *
 */
case class Preenrolment[T, R, UT] (

  id:Id[Preenrolment[T, R, UT],String],

  name: Option[String] = None,

  rows: Seq[Preenrolment.Row[T, R, UT]] = Seq.empty,

  created: Long = System.currentTimeMillis,

  modified: Long = System.currentTimeMillis
) extends HasStringId[Preenrolment[T, R, UT]]

object Preenrolment {
  case class Row[T, R, UT](
    roles: Set[R] = Set.empty,
    target: Id[T, String],
    identity: IdentityLookup,
    used: Option[Used[Id[UT, String]]] = None
  )
}
