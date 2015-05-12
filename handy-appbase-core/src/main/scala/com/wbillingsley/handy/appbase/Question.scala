package com.wbillingsley.handy.appbase

import com.wbillingsley.handy.{Id, HasStringId, HasKind}

abstract class Question(val kind:String) extends HasKind with HasStringId[Question]

abstract class Answer[T](val kind:String) extends HasKind {

  val question: Id[Question,String]

  var answer: Option[T]

}


object ShortTextQuestion {
  val kind = "Short text"
}

case class ShortTextQuestion(

  id:Id[Question,String],

  prompt: String,

  maxLength: Option[Int] = None

) extends Question(ShortTextQuestion.kind)


case class ShortTextAnswer(
  question: Id[Question, String],

  var answer: Option[String]
) extends Answer[String](ShortTextQuestion.kind)


object BooleanQuestion {
  val kind = "Boolean"
}

case class BooleanQuestion(

  id:Id[Question,String],

  prompt: String

) extends Question(BooleanQuestion.kind)


case class BooleanAnswer(
  question: Id[Question, String],

  var answer: Option[Boolean]
) extends Answer[Boolean](BooleanQuestion.kind)

