package com.wbillingsley.handy.appbase

import com.wbillingsley.handy.{Id, HasKind, HasStringId}

case class Course (

    id:Id[Course, String],

    title: Option[String] = None,

    shortName:Option[String] = None,

    shortDescription:Option[String] = None,

    website:Option[String] = None,

    coverImage:Option[String] = None,

    secret: String = scala.util.Random.alphanumeric.take(16).mkString,

    ltis: Seq[LTIConsumer] = Seq.empty,

    addedBy:Id[Course.Reg, String],

    created:Long = System.currentTimeMillis

) extends HasStringId[Course]

object Course {
  type Reg = Registration[Course, CourseRole, HasKind]
  type Preenrol = Preenrolment[Course, Course, CourseRole, Course.Reg]
  type PreenrolRow = Preenrolment.Row[Course, CourseRole, Course.Reg]
}

case class CourseRole(r:String)

object CourseRole {

  val staff = CourseRole("staff")

  val student = CourseRole("student")

  val roles:Set[CourseRole] = Set(staff, student)

}

case class LTIConsumer(clientKey:String, secret:String, comment:Option[String] = None)