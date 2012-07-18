/*
This file is available under the MIT licence:

Copyright (C) 2012 William Billingsley

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
  documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit
  persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
  Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
  OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
package com.wbillingsley.handy

import scala.collection.mutable

case class PermissionWasRefused[T](resp:PermResponse) extends Exception

abstract class Permission[T] {
  def resolve(who: T):PermResponse
}

sealed abstract class PermResponse {
  def unapply:Option[(Boolean, String)]

  def toTuple:(Boolean, String)

  def may: Boolean
  
  def reason:String

  def perform[A >: RefFailed](f: => A):A
}

case class PermApproved(reason: String) extends PermResponse {
  def unapply = Some(true, reason)

  def toTuple:(Boolean, String) = (true, reason)

  def may = true

  def perform[A >: RefFailed](f: => A):A = {
    f
  }
}

case class PermRefused(reason: String) extends PermResponse {
  def unapply = Some(false, reason)

  def toTuple:(Boolean, String) = (false, reason)

  def may = false

  def perform[A >: RefFailed](f: => A):A = {
    RefFailed(reason, Some(PermissionWasRefused(this)))
  }
}

class PermissionToken[T](val who: T) {

  private val approvals = mutable.Map.empty[Permission[T], PermApproved]

  private var status:PermResponse = PermApproved("No permissions have been asked for")

  /**
   * Requests a permission; if it is denied this token goes into a refused state
   * @param perm
   * @return
   */
  def request(perm:Permission[T]) = {
    if (may) {
      status = approvals.getOrElse(perm, check(perm))
    }
    this
  }

  /**
   * Checks if a permission is approved, but only remembers a success not a failure.
   * (So future requests for permissions are not denied)
   * @param perm
   * @return
   */
  def check(perm:Permission[T]) = {
    val pr = perm.resolve(who)
    pr match {
      case pa:PermApproved => approvals.put(perm, pa)
      case _ => { /* do nothing */ }
    }
    pr
  }
  
  def may = status match {
    case pa:PermApproved => true
    case pr:PermRefused => false
  }

  def perform[A >: RefFailed](f: => A):A = {
    status.perform(f)
  }

  def reason = status.reason

  def toTuple = status.toTuple
  
  def either:Either[PermRefused, PermApproved] = status match {
    case pa:PermApproved => Right(pa)
    case pr:PermRefused => Left(pr)
  } 

}
