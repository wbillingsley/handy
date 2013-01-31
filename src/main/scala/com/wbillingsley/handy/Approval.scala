package com.wbillingsley.handy

import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}
import Ref._

/**
 * A mutable set of permissions.  It's mutable so that in the case where
 * we ask for, but do not require, a permission we can cache the 
 * permission even if the prgrammer didn't assign the return typee
 */
case class Approval[T](val who: Ref[T], val permissions: mutable.Set[Perm[T]] = mutable.Set.empty[Perm[T]]) {
  
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
 * This overrides the equals operator to consider references equal if they have the same class and the
 * same id.  So, CanEdit(RefItself(Page1)) == CanEdit(RefById(classof[Page], 1))
 */
abstract class PermOnIdRef[U, +T, K](what: Ref[T])(implicit g:GetsId[T, K]) extends Perm[U] {
  
  override def equals(other:Any) = {
    (this.getClass == other.getClass) && {
      what.sameId(other.asInstanceOf[Ref[T]])(g)
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
    
  }
  
  
  implicit def refApproval[T](a: Approval[T]):RefItself[Approval[T]] = RefItself(a)
  
  implicit def wrapApproval[T](a: Approval[T]):WrappedRefApproval[T] = WrappedRefApproval(RefItself(a))
  
}
