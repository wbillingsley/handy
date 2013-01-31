package com.wbillingsley.handy.realistic

import com.wbillingsley.handy._
import Ref._

/**
 * Different DB/persistence mechanisms might store things quite differently, but
 * they all should be fairly easily representable in terms of Ref.
 * 
 * We're just going to show this in this example by storing Page.createdBy by its user id.
 * 
 * But we're also going to show how different classes can come from different databases by
 * making page (but not user) return a future.
 */
object DB {

  case class DBUser(val id: Int, var name: String, var roles: Set[Role] = Set(Viewer)) extends User
  case class DBPage(val id: Int, var _createdBy: Option[Int], var content: String, var isPublic: Boolean) extends Page {

    def createdBy = RefById(classOf[User], _createdBy)

    def createdBy_=(u: Ref[User]) {
      _createdBy = u.getId
    }

  }

  import scala.collection.mutable

  val userTable = mutable.Map(
    1 -> DBUser(1, "An admin", Set(Viewer, Editor, Admin)),
    2 -> DBUser(2, "An editor", Set(Viewer, Editor)),
    3 -> DBUser(3, "A viewer", Set(Viewer)))

  val pageTable = mutable.Map(
    1 -> DBPage(1, Some(1), "A public page", true),
    2 -> DBPage(2, Some(1), "A non-public page", false))

  def createPage(createdBy: Ref[User], content: String, isPublic: Boolean = false) = {
    val key = pageTable.keySet.max + 1
    val p = DBPage(key, createdBy.getId, content, isPublic)
    pageTable.put(key, p)
    p.itself
  }
  
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  
  /**
   * We are resolving users synchronously, but pages as futures. That's just to show that the handy framework can cope!
   */
  def resolvers:Map[Class[_], (Int) => Ref[AppItem]] = Map(          
    classOf[User] -> { (id:Int) => Ref(userTable.get(id)) },
    classOf[Page] -> { (id:Int) => new RefFuture( future { pageTable(id) } ) }
  )
  
  def resolve[T](clazz: Class[T], id:Int) = {
    resolvers.get(clazz) match {
      case Some(resolver) => resolver(id).asInstanceOf[Ref[T]]
      case _ => RefFailed(new IllegalArgumentException("I don't know how to resolve references to that class"))
    }
  }
  
}