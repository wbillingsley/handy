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

  import scala.collection.mutable

  val userTable = mutable.Map(
    1 -> DBUser(1, "An admin", Set(Viewer, Editor, Admin)),
    2 -> DBUser(2, "An editor", Set(Viewer, Editor)),
    3 -> DBUser(3, "A viewer", Set(Viewer)))

  val pageTable = mutable.Map(
    1 -> DBPage(1, Some(1), "A public page", true),
    2 -> DBPage(2, Some(1), "A non-public page", false))
  
  /**
   * We are resolving users synchronously, but pages as futures. That's just to show that the handy framework can cope!
   */
  implicit object User extends LookUp[User, Int] {
    def lookUpOne[K <: Int](r:RefById[User, K]) = Ref(userTable.get(r.id))
    
    def lookUpMany[K <: Int](r:RefManyById[User, K]) = {
      (for {
        id <- r.rawIds
        u <- userTable.get(id)
      } yield u).toRefMany
    }
  }

  implicit object Page extends LookUp[Page, Int] {
    import scala.concurrent._
    import ExecutionContext.Implicits.global    
    
    def lookUpOne[K <: Int](r:RefById[Page, K]) = new RefFuture( future { pageTable(r.id) } )
    
    def lookUpMany[K <: Int](r:RefManyById[Page, K]) = {
      (for {
        id <- r.rawIds
        u <- pageTable.get(id)
      } yield u).toRefMany
    }
  }

  case class DBUser(val id: Int, var name: String, var roles: Set[Role] = Set(Viewer)) extends User
  case class DBPage(val id: Int, var _createdBy: Option[Int], var content: String, var isPublic: Boolean) extends Page {

    def createdBy = Ref.fromOptionId[User, Int](_createdBy)

    def createdBy_=(u: Ref[User]) {
      _createdBy = u.getId
    }

  }

  def createPage(createdBy: Ref[User], content: String, isPublic: Boolean = false) = {
    val key = pageTable.keySet.max + 1
    val p = DBPage(key, createdBy.getId, content, isPublic)
    pageTable.put(key, p)
    p.itself
  }
  
}