package com.wbillingsley.handy.realistic

import com.wbillingsley.handy._
import Ref._

/*
 * Model classes for our sample app
 */
abstract class Role
case object Viewer extends Role
case object Editor extends Role
case object Admin extends Role

trait AppItem extends HasId[Int]

object AppItem {

  /**
   * Normally this would be part of your database classes, but here we're being lazy and making Int IDs 
   * part of the API.
   */
  implicit object gg extends GetsId[HasId[Int], Int] {
    def getId(obj: HasId[Int]) = Some(obj.id)

    def canonical(key: Any) = key match {
      case k: Int => Some(k)
      case _ => None
    }
  }
}

trait User extends AppItem { val id: Int; var name: String; var roles: Set[Role] }
trait Page extends AppItem { val id: Int; def createdBy: Ref[User]; var content: String; var isPublic: Boolean }

