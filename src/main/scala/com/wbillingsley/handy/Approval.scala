package com.wbillingsley.handy

import scala.collection.mutable
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
    val who: Ref[U],
    val permissions: mutable.Set[Perm[U]] with mutable.SynchronizedSet[Perm[U]] = new mutable.HashSet[Perm[U]] with mutable.SynchronizedSet[Perm[U]],
    val cache:LookUpCache = new LookUpCache
) {
  
}

case class Refused(msg: String) extends Throwable(msg) {  
  def toRef = RefFailed(this)
}

object Refused {
  implicit def toRef(r:Refused) = RefFailed(r)
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
  implicit def toRef(a:Approved) = RefItself(a)
  
  
}

trait Perm[U] {
  def resolve(prior: Approval[U]):Ref[Approved]
}

object Perm {

  implicit def toRefPerm[U](p:Perm[U]) = p.itself

  /**
   * Creates a unique permission object
   */
  def unique[U](block: (Approval[U]) => Ref[Approved]) = new Perm[U] {
    def resolve(prior: Approval[U]) = block(prior)
  }

  /**
   * Allows permissions that will cache approvals according to an item's id. For example
   *
   * <pre>{@code
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
  def cacheOnId[U, T](block: (Approval[U], Ref[T]) => Ref[Approved])(implicit g:GetsId[T, _]) = {
    new PermissionGenerator(block)(g)
  }

  class PermissionGenerator[U, T, K](resolve: (Approval[U], Ref[T]) => Ref[Approved])(implicit g:GetsId[T, K]) {

    /**
     * Two generated permissions are considered the same if they came from the same generator and have the same ID.
     * This case class provides an easy way to ensure that.
     */
    case class InnerEquality[K](id:K)

    class POI(id:K, r:Ref[T]) extends Perm[U] {
      val eq = InnerEquality(id)

      override def equals(o:Any) = o match {
        case p:PermissionGenerator.this.POI => eq == p.eq
        case _ => false
      }

      override def hashCode = eq.hashCode

      def resolve(prior: Approval[U]): Ref[Approved] = PermissionGenerator.this.resolve(prior, r)
    }

    def apply(r:Ref[T]):Ref[Perm[U]] = for { k <- r.refId } yield new POI(k, r)
  }

}


/**
 * Older longhand way of getting a permission that will cache the approval depending on its ID.
 *
 * A permission on a reference that has a gettable id.  For instance CanEdit(page: Ref[Page]).
 * This overrides the equals operator to consider two permissions equal if they are the same permission
 * and have the same idea  
 * So, CanEditPage(RefItself(Page1)) == CanEditPage(RefById(classof[Page], 1))
 * 
 * Beware, however, that by default, equality only considers the ID, and not the type T, unless clazz is specified 
 * So, PermOnIdRef(RefById(classof[Page], 1)) == PermOnIdRef(RefById(classof[User], 1))
 * But PermOnIdRef(RefById(classof[Page], 1), classOf[Page]) != PermOnIdRef(RefById(classof[User], 1), classOf[User])
 * 
 */
abstract class PermOnIdRef[U, T](what:Ref[T], clazz:Class[_ <: T] = classOf[Nothing])(implicit g:GetsId[T, _]) {

  def resolve(prior: Approval[U]): Ref[Approved]

  def toRefPerm:Ref[Perm[U]] = for {
    k <- what.refId
  } yield new PermOnIdRef.POI(getClass(), k, clazz)({
    prior => resolve(prior)
  })

}

object PermOnIdRef {

  class POI[U, T, K](val permClazz: Class[_], val id:K, val clazz:Class[_])(val block: (Approval[U]) => Ref[Approved]) extends Perm[U] {

    val eq:(Class[_], K, Class[_]) = (permClazz, id, clazz)

    override def equals(o:Any) = {
      o match {
        case p:POI[_,_,_] => p.eq == eq
        case _ => false
      }
    }

    override def hashCode = eq.hashCode()

    def resolve(prior: Approval[U]): Ref[Approved] = block(prior)
  }

  implicit def toRP[U](p:PermOnIdRef[U, _]) = p.toRefPerm
}


object Approval {
  
  import scala.language.implicitConversions
  
  implicit class WrappedRefApproval[U](val ra: Ref[Approval[U]]) extends AnyVal {
    
    def askOne (permission: Perm[U]):Ref[Approved] = {
      ra flatMap { a =>
        if (a.permissions contains permission)
          RefItself(Approved("Already approved"))
        else {
          val pa = permission.resolve(a)

          /*
           * Cache and return the permission. Do this inside flatMap to ensure it has been cached before anything
           * else happens (monadic sequencing)
           */
          pa flatMap { approved =>
            a.permissions.add(permission)
            pa
          }
        }
      }      
    }
/*
    def ask(permissions: Perm[U]*):Ref[Approved] = {
      ra flatMap { approval =>
        permissions.foldLeft[Ref[Approved]](Approved("Nothing to approve") itself) { (refApproved, perm) =>
        // Check if we need to approve anything. If we've already failed, or there's no permission, we don't
          val toApprove = refApproved map { _ => perm }

          // Return its approval
          toApprove flatMap { p => askOne(p) }
        }
      }
    }*/
    
    def ask(permissions: Ref[Perm[U]]*):Ref[Approved] = {
      ra flatMap { approval =>
        permissions.foldLeft[Ref[Approved]](Approved("Nothing to approve") itself) { (refApproved, refPerm) =>
          // Check if we need to approve anything. If we've already failed, or there's no permission, we don't
          val toApprove = refApproved flatMap { _ => refPerm }

          // Return its approval
          toApprove flatMap { p => askOne(p) }
        }
      }
    }

    /**
     * Returns a Ref[Boolean] 
     */
    def askBoolean(permission: Perm[U]):Ref[Boolean] = {
      askOne(permission) map(_ => true) recoverWith(PartialFunction.apply(
        (x:Throwable) => false.itself)
      ) orIfNone false.itself
    }

    /**
     * Returns a Ref[Boolean]
     */
    def askBoolean(refPerm: Ref[Perm[U]]):Ref[Boolean] = refPerm flatMap askBoolean // calls askBoolean(Perm[U])
  }
  
  
  implicit def refApproval[U](a: Approval[U]):RefItself[Approval[U]] = RefItself(a)
  
  implicit def wrapApproval[U](a: Approval[U]):WrappedRefApproval[U] = WrappedRefApproval(RefItself(a))
  
}
