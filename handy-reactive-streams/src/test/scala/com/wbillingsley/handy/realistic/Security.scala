package com.wbillingsley.handy.realistic

import com.wbillingsley.handy._

object Security {
  def hasRole(user: RefOpt[User], role: Role): RefOpt[Approved] = {
    (for {
      u <- user if u.roles contains role
    } yield Approved("Is editor")) orElse RefOptFailed(Refused("You do not have the role " + role))
  }

  val canRead = Perm.of[User, Page].onId {
    case (prior, page) =>
      for {
        p <- page
        a <- if (p.isPublic) {
          Approved("Public pages can be read by anyone").itself
        } else hasRole(prior.who, Viewer).require
      } yield a
  }

  val canEdit = Perm.of[User, Page].onId {
    case (prior, page) =>
      hasRole(prior.who, Editor).require
  }

  val canCreate = Perm.unique[User] {
    case (prior) =>
      hasRole(prior.who, Editor).require
  }
}
