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
}

import AppItem.gg

case class CanRead(page: Ref[Page]) extends PermOnIdRef[User, Page](page) {

  def resolve(prior: Approval[User]): Ref[Approved] = {
    (for (p <- page if p.isPublic) yield 
      Approved("Public pages can be read by anyone") 
    ) orIfNone hasRole(prior.who, Viewer)
  }
  
}

case class CanEdit(page: Ref[Page]) extends PermOnIdRef[User, Page](page) {

  def resolve(prior: Approval[User]): Ref[Approved] = hasRole(prior.who, Editor)

}

case object CanCreate extends Perm[User] {

  def resolve(prior: Approval[User]): Ref[Approved] = hasRole(prior.who, Editor)  

}