package com.wbillingsley.handy

import scala.collection.mutable

/**
 * A mutable set of permissions.  It's mutable so that in the case where
 * we ask for, but do not require, a permission we can cache the 
 * permission even if the prgrammer didn't assign the return typee
 */
case class Approval[T](val who: T, val permissions: mutable.Set[Perm[T]] = mutable.Set.empty[Perm[T]]) 

case class Refused(msg: String) extends Throwable(msg) {  
  implicit def toRef = RefFailed(this)
}

/** 
 * Permissions should return RefItself(Approved) if they decide that yes the user may do that.  
 * There is an implicit conversion from Approved to RefItself(Approved), so they can also just return
 * Approved. 
 */
case class Approved(msg: String = "Approved") {
  
  implicit def toRef = RefItself(this)
  
}

abstract class Perm[T] {
  def resolve(prior: Approval[T]):Ref[Approved]
}

/**
 * A permission on a reference that has a gettable id.  For instance CanEdit(page: Ref[Page]).
 * This overrides the equals operator to consider references equal if they have the same class and the
 * same id.  So, CanEdit(RefItself(Page1)) == CanEdit(RefById(classof[Page], 1))
 */
abstract class PermOnIdRef[T, K](what: Ref[T])(implicit g:GetsId[T, K]) extends Perm[T] {
  
  override def equals(other:Any) = {
    (this.getClass == other.getClass) && {
      what.sameId(other.asInstanceOf[Ref[T]])(g)
    }
  }
  
}


object Approval {
  
  import scala.language.implicitConversions
  
  implicit class WrappedRefApproved(ra: Ref[Approved]) {
    
    def perform[B](f: => Ref[B]):Ref[B] = {
      ra flatMap { a => f }
    }
    
  }
  
  implicit class WrappedRefApproval[T](ra: Ref[Approval[T]]) {
    
    def ask(permission: Perm[T]):Ref[Approved] = {
      
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
    
  }
  
  implicit def refApproval[T](a: Approval[T]) = RefItself(a)
  
  implicit def wrapApproval[T](a: Approval[T]) = new WrappedRefApproval(RefItself(a))
  
}
