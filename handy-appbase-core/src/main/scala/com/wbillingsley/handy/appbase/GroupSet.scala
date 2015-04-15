package com.wbillingsley.handy.appbase

import com.wbillingsley.handy.{Id, HasStringId}


case class GroupSet (

  id:Id[GroupSet,String],

  name:Option[String] = None,

  description:Option[String] = None,

  course: Id[Course,String],

  parent: Option[Id[GroupSet, String]] = None,

  preenrol: Option[Group.Preenrol] = None,

  created: Long = System.currentTimeMillis

) extends HasStringId[GroupSet]
