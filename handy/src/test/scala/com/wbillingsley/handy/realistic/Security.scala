package com.wbillingsley.handy.realistic

import com.wbillingsley.handy._
import com.wbillingsley.handy.Ref._

import Security._


/* 
 * Your Security classes
 */

object Security {
  def hasRole(user: Ref[User], role: Role) = {
    (for (u <- user if u.roles contains role)
      yield Approved("Is editor")) orIfNone Refused("You do not have the role " + role)
  }

  val canRead = Perm.of[User,Page].onId {
    case (prior, page) =>
      (for (p <- page if p.isPublic) yield
        Approved("Public pages can be read by anyone")
        ) orIfNone hasRole(prior.who, Viewer)
  }

  val canEdit = Perm.of[User,Page].onId {
    case (prior, page) =>
      hasRole(prior.who, Editor)
  }

  val canCreate = Perm.unique[User] {
    case (prior) =>
      hasRole(prior.who, Editor)
  }
}