package com.wbillingsley.handy

import scala.language.{implicitConversions, postfixOps}
import Ref._


/**
 * A mutable set of permissions.  It's mutable so that in the case where
 * we ask for, but do not require, a permission we can cache the
 * permission even if the programmer didn't assign the return types
 *
 * Approvals also have a LookUpCache, which can set and used to avoid
 * calling the database mutliple times for the same item.
 */
case class Approval[U](
    who: RefOpt[U]
) {
  import java.util._

  private val permissions = new concurrent.ConcurrentHashMap[Perm[U], Approved]()

  val cache:LookUpCache = new LookUpCache

  def get(permission:Perm[U]) = Option(permissions.get(permission))

  def add(permission:Perm[U], approved:Approved):Approved = {
    permissions.put(permission, approved)
    approved
  }

  def askAfresh(permission:Perm[U]):Ref[Approved] = permission.resolve(this)

  def askOne(permission:Perm[U]):Ref[Approved] = {
    get(permission).map(Ref.itself).getOrElse {
      for (appr <- permission.resolve(this)) yield add(permission, appr)
    }
  }

  def ask(rps: Ref[Perm[U]]*):Ref[Approved] = {
    for {
      approved <- rps.foldLeft[Ref[Approved]](Approved("Nothing to approve").itself) {
        (rApproved, rp) => for { p <- rp; appr <- askOne(p) } yield appr
      }
    } yield approved
  }

  /**
   * Returns a Ref[Boolean]
   */
  def askBoolean(permission: Perm[U]):Ref[Boolean] = {
    askOne(permission) map(_ => true) recoverWith { case _ => false.itself }
  }

}

case class Refused(msg: String) extends Throwable(msg) {
  def toRef = RefFailed(this)
}

object Refused {
  implicit def toRef(r:Refused):RefFailed = RefFailed(r)
}

/**
 * Permissions should return RefItself(Approved) if they decide that yes the user may do that.
 * There is an implicit conversion from Approved to RefItself(Approved), so they can also just return
 * Approved.
 */
case class Approved(msg: String = "Approved") {
  def toRef = RefItself(this)
}

object Approved {
  implicit def toRef(a:Approved):Ref[Approved] = RefItself(a)
}

/**
 * A permission that can be approved or denied
 * @tparam U the user class that this is approved for
 */
trait Perm[U] {
  def resolve(prior: Approval[U]):Ref[Approved]
}

object Perm {

  implicit def toRefPerm[U](p:Perm[U]):Ref[Perm[U]] = p.itself

  /**
   * Creates a unique permission object
   */
  def unique[U](block: (Approval[U]) => Ref[Approved]):Perm[U] = new Perm[U] {
    def resolve(prior: Approval[U]) = block(prior)
  }

  /**
   * Allows permissions that will cache approvals according to an item's id. For example
   *
   * <pre>{
   *   val createPage = Perm.cacheOnId[User, Page] { case (prior, refPage) =>
   *     ...
   *   }
   *
   *   approval ask createPage(page1.itself)
   *
   *   // This would find the remembered approval
   *   approval ask createPage(LazyId(1).of[Page])
   * }</pre>
   *
   */
  def onId[U, T, K](block: (Approval[U], Ref[T]) => Ref[Approved])(implicit g:GetsId[T, K]) = {
    new PermissionGenerator(block)(g)
  }

  /**
   * Allows Perm[U,T].onId {  } notation with an inferred key type
   */
  def of[U,T] = new Typed[U,T]
  def apply[U,T] = of[U,T]
  class Typed[U,T] {
    def onId[K](block: (Approval[U], Ref[T]) => Ref[Approved])(implicit g:GetsId[T, K]) = Perm.onId[U,T,K](block)(g)
  }


  class PermissionGenerator[U, T, K](resolve: (Approval[U], Ref[T]) => Ref[Approved])(implicit g:GetsId[T, K]) {

    import Id._

    /**
     * Two generated permissions are considered the same if they came from the same generator and have the same ID.
     * This case class provides an easy way to ensure that.
     */
    case class InnerEquality[K](id:K)

    class POI(id:Id[T,K], r:Ref[T]) extends Perm[U] {
      val eq = InnerEquality(id)

      override def equals(o:Any) = o match {
        case p:PermissionGenerator.this.POI => eq == p.eq
        case _ => false
      }

      override def hashCode = eq.hashCode

      def resolve(prior: Approval[U]): Ref[Approved] = PermissionGenerator.this.resolve(prior, r)
    }

    def apply(r:Ref[T]):Ref[Perm[U]] = for { k <- r.refId.require } yield new POI(k, r)

    def apply(id:Id[T,K])(implicit lu:LookUp[T, K]):Ref[Perm[U]] = new POI(id, id.lookUp(lu))

    def apply(oid:Option[Id[T,K]])(implicit lu:LookUp[T, K]):RefOpt[Perm[U]] = {
      RefOpt(oid).flatMap(apply)
    }
  }

}


object Approval {

  import scala.language.implicitConversions

  implicit class WrappedRefApproval[U](val ra: Ref[Approval[U]]) extends AnyVal {

    def askOne (permission: Perm[U]):Ref[Approved] = {
      for {
        a <- ra
        approved <- a.askOne(permission)
      } yield approved
    }

    def ask(rps: Ref[Perm[U]]*):Ref[Approved] = {
      for {
        a <- ra
        approved <- rps.foldLeft[Ref[Approved]](Approved("Nothing to approve").itself) {
          (rApproved, rp) => for { p <- rp; appr <- a.askOne(p) } yield appr
        }
      } yield approved
    }

    /**
     * Returns a Ref[Boolean]
     */
    def askBoolean(permission: Perm[U]):Ref[Boolean] = {
      for {
        a <- ra
        approved <- a.askBoolean(permission)
      } yield approved
    }

    /**
     * Returns a Ref[Boolean]
     */
    def askBoolean(refPerm: Ref[Perm[U]]):Ref[Boolean] = refPerm flatMap askBoolean // calls askBoolean(Perm[U])
  }

  implicit def refApproval[U](a: Approval[U]):RefItself[Approval[U]] = RefItself(a)

  implicit def wrapApproval[U](a: Approval[U]):WrappedRefApproval[U] = WrappedRefApproval(RefItself(a))

}
