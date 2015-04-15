package com.wbillingsley.handy.appbase

import com.wbillingsley.handy.{Id, EmptyKind, HasStringId, HasKind}


case class Task[B <: HasKind](

   id: Id[Task[B],String],

   course: Id[Course, String],

   details: TaskDetails = new TaskDetails(),

   body: B = EmptyKind

) extends HasStringId[Task[B]] {

  def kind = body.kind

}

case class TaskDetails (

   name:Option[String] = None,

   description:Option[String] = None,

   created: Long = System.currentTimeMillis,

   published: Option[Long] = Some(System.currentTimeMillis),

   due: Option[Long] = None
 )

