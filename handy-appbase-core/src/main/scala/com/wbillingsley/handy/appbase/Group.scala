package com.wbillingsley.handy.appbase

import com.wbillingsley.handy.{HasKind, Ids, HasStringId, Id}

case class Group (

   id:Id[Group,String],

   parent:Option[Id[Group, String]] = None,

   course:Option[Id[Course, String]] = None,

   set:Id[GroupSet,String],

   name:Option[String] = None,

   provenance:Option[String] = None,

   members:Ids[User, String] = Ids.empty,

   created:Long = System.currentTimeMillis

 ) extends HasStringId[Group]


object Group {
  type Reg = Registration[Group, GroupRole, HasKind]
  type Preenrol = Preenrolment[Group, GroupRole, Group.Reg]
}

case class GroupRole(r:String)

object GroupRole {
  val member = CourseRole("member")
  val roles:Set[CourseRole] = Set(member)
}

