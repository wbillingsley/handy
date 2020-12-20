package com.wbillingsley.handy.realistic

import com.wbillingsley.handy._

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

  /**
    * We are resolving users synchronously, but pages as futures. That's just to show that the handy framework can cope!
    */
  implicit object User extends LookUp[User, Int] {
    def one[K <: Int](r: Id[User, K]) = RefOpt(userTable.get(r.id)).require

    def many[K <: Int](r: Ids[User, K]) = {
      (for {
        id <- r.ids
        u <- userTable.get(id)
      } yield u).toRefMany
    }
  }

  implicit object Page extends LookUp[Page, Int] {

    import scala.concurrent._
    import ExecutionContext.Implicits.global

    def one[K <: Int](r: Id[Page, K]) = new RefFuture(Future {
      pageTable(r.id)
    })

    def many[K <: Int](r: Ids[Page, K]) = {
      (for {
        id <- r.ids
        u <- pageTable.get(id)
      } yield u).toRefMany
    }
  }


  val userTable = mutable.Map(
    1 -> DBUser(1, "An admin", Set(Viewer, Editor, Admin)),
    2 -> DBUser(2, "An editor", Set(Viewer, Editor)),
    3 -> DBUser(3, "A viewer", Set(Viewer)))

  val pageTable = mutable.Map(
    1 -> DBPage(1, LazyId(1).of[User], "A public page", true),
    2 -> DBPage(2, LazyId(1).of[User], "A non-public page", false))

  import Id._

  case class DBUser(val _id: Int, var name: String, var roles: Set[Role] = Set(Viewer)) extends User {
    def id = _id.asId[DBUser]
  }

  case class DBPage(val _id: Int, var _createdBy: Ref[User], var content: String, var isPublic: Boolean) extends Page {

    def id = _id.asId[DBPage]

    def createdBy = _createdBy

    def createdBy_=(u: Ref[User]): Unit = {
      val rId = u.refId.require
      _createdBy = rId flatMap { id => id.lazily }
    }

  }

  def createPage(createdBy: Ref[User], content: String, isPublic: Boolean = false) = {
    val key = pageTable.keySet.max + 1
    val lazyCB = createdBy.refId.require flatMap { id => id.lazily }

    val p = DBPage(key, lazyCB, content, isPublic)
    pageTable.put(key, p)
    p.itself
  }

}
