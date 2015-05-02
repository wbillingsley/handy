package com.wbillingsley.handy.appbase

import com.wbillingsley.handy.{HasKind, Ids, HasStringId, Id}

case class Group (

   id:Id[Group,String],

   parent:Option[Id[Group, String]] = None,

   course:Option[Id[Course, String]] = None,

   set:Id[GroupSet,String],

   name:Option[String] = None,

   provenance:Option[String] = None,

   members:Ids[Group.Reg, String] = Ids.empty,

   created:Long = System.currentTimeMillis

 ) extends HasStringId[Group]


object Group {
  type Reg = Registration[Group, GroupRole, HasKind]
  type Preenrol = Preenrolment[GroupSet, Group, GroupRole, Group.Reg]
  type PreenrolRow = Preenrolment.Row[Group, GroupRole, Group.Reg]
}

case class GroupRole(r:String)

object GroupRole {
  val member = GroupRole("member")
  val roles:Set[GroupRole] = Set(member)
}

