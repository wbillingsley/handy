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
case class Approval[T](
    val who: Ref[T], 
    val permissions: mutable.Set[Perm[T]] = mutable.Set.empty[Perm[T]],
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

abstract class Perm[T] {
  def resolve(prior: Approval[T]):Ref[Approved]
}

/**
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
abstract class PermOnIdRef[U, T](val what: Ref[T], val clazz: Class[_ <: T] = classOf[Nothing])(implicit val g:GetsId[T, _]) extends Perm[U] {
  
  override def hashCode = {
    (getClass, what.getId, clazz).hashCode
  }
  
  override def equals(other:Any) = {
    (this.getClass == other.getClass) && {
      val o = other.asInstanceOf[PermOnIdRef[U, T]]
      clazz == o.clazz && what.getId(g) == o.what.getId(g)
    }
  }
  
}


object Approval {
  
  import scala.language.implicitConversions
  
  implicit class WrappedRefApproval[T](val ra: Ref[Approval[T]]) extends AnyVal {
    
    def askOne (permission: Perm[T]):Ref[Approved] = {
      ra flatMap { a =>
        if (a.permissions contains permission)
          RefItself(Approved("Already approved"))
        else {
          val pa = permission.resolve(a)
          for (approved <- pa) { a.permissions.add(permission) }
          pa
        }
      }      
    }
    
    def ask(permissions: Perm[T]*):Ref[Approved] = {
      ra flatMap { approval =>
      	permissions.foldLeft[Ref[Approved]](Approved("Nothing to approve") itself){ (appr, perm) => appr.flatMap{ a => askOne(perm) } }
      }
    }
    
    /**
     * Returns a Ref[Boolean] 
     */
    def askBoolean(permission: Perm[T]) = askOne(permission) map(_ => true) recoverWith(PartialFunction.apply((x:Throwable) => false.itself)) orIfNone false.itself
    
  }
  
  
  implicit def refApproval[T](a: Approval[T]):RefItself[Approval[T]] = RefItself(a)
  
  implicit def wrapApproval[T](a: Approval[T]):WrappedRefApproval[T] = WrappedRefApproval(RefItself(a))
  
}
