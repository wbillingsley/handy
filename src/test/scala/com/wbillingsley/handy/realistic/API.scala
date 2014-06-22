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

trait AppItem[T] extends HasId[T, Int]

object AppItem {

  /**
   * Normally this would be part of your database classes, but here we're being lazy and making Int IDs 
   * part of the API.
   */
  implicit def gg[T] = new GetsId[HasId[T, Int], Int] {
    import Id._

    def getId[TT <: HasId[T, Int]](obj: TT) = {
      val id = obj.id.id
      Some(id.asId[TT])
    }

    def canonical[TT <: HasId[T, Int]](key: Any) = key match {
      case k: Int => Some(k.asId[TT])
      case _ => None
    }
  }
}

trait User extends AppItem[User] { def id: Id[User,Int]; var name: String; var roles: Set[Role] }
trait Page extends AppItem[Page] { def id: Id[Page,Int]; def createdBy: Ref[User]; var content: String; var isPublic: Boolean }

