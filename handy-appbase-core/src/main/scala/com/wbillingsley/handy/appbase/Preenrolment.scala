package com.wbillingsley.handy.appbase

import com.wbillingsley.handy.{HasStringId, Id}

/**
 * Lists that when a user with a particular identity is seen, they should be
 * registered automatically for something.
 *
 * @param id ID of this Preenrolment
 * @param within Context, used to limit the search. For example, a group pre-enrolment may be within a particular course.
 * @param name Name of the preenrolment
 * @param rows
 * @param created
 * @param modified
 * @tparam W within
 * @tparam T what a user is being enrolled for
 * @tparam R the roles the user may be given
 * @tparam RT the resulting registration type
 */
case class Preenrolment[W, T, R, RT] (

  id:Id[Preenrolment[W, T, R, RT],String],

  within: Option[Id[W, String]] = None,

  name: Option[String] = None,

  rows: Seq[Preenrolment.Row[T, R, RT]] = Seq.empty,

  created: Long = System.currentTimeMillis,

  modified: Long = System.currentTimeMillis
) extends HasStringId[Preenrolment[W, T, R, RT]]

object Preenrolment {

  /**
   * A row of a pre-enrolment listing
   * @param roles the roles to grant this user
   * @param target what they are to be enrolled in
   * @param identity how the user will be identified
   * @param used whether this row has been used (and created a registration)
   * @tparam T
   * @tparam R
   * @tparam UT
   */
  case class Row[T, R, UT](
    roles: Set[R] = Set.empty,
    target: Id[T, String],
    identity: IdentityLookup,
    used: Option[Used[UT]] = None
  )
}
