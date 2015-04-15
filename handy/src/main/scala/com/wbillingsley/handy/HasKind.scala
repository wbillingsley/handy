package com.wbillingsley.handy

/**
 * Indicates a type has a "kind" string.
 *
 * Primarily this is used where we have collections of slightly disparate objects.
 * For example, a course might contain many different kinds of assessment item. Although at
 * runtime, we can use the type of the object to determine what it is, when we receive it in
 * serialised form (from a database, or from JSON) we need to know what kind it is in order
 * to know what method to call to recreate the object.
 *
 * The simple solution I often use is to put it in a string -- that way it can easily be
 * inspected from the JSON or the object alike.
 */
trait HasKind {
  def kind: String
}

object EmptyKind extends HasKind {
  val kind = "empty"
}

case class FaultyKind(t:Throwable) extends HasKind {
  val kind = "faulty"
}
