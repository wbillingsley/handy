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

case class PermissionWasRefused[T](token:PermissionToken[T]) extends Exception

abstract class Permission[T] {
  def resolve(who: T):PermResponse
}

sealed abstract class PermResponse {
  def unapply:Option[(Boolean, String)]

  def toTuple:(Boolean, String)
  
  def reason:String
}

case class PermApproved(reason: String) extends PermResponse {
  def unapply = Some(true, reason)

  def toTuple:(Boolean, String) = (true, reason)
}

case class PermRefused(reason: String) extends PermResponse {
  def unapply = Some(false, reason)

  def toTuple:(Boolean, String) = (false, reason)
}

class PermissionToken[T](val who: T) {

  private val approvals = mutable.Map.empty[Permission[T], PermApproved]

  private var status:PermResponse = PermApproved("No permissions have been asked for")
  
  def request(perm:Permission[T]) = {
    if (may) {
      status = approvals.getOrElse(perm, {
        val pr = perm.resolve(who)
        pr match {
          case pa:PermApproved => approvals.put(perm, pa)
          case _ => { /* do nothing */ }
        }
        pr
      })
    }
    this
  }
  
  def may = status match {
    case pa:PermApproved => true
    case pr:PermRefused => false
  }

  def perform[A >: RefFailed](f: => A):A = {
    status match {
      case pa:PermApproved => f
      case pr:PermRefused => RefFailed(pr.reason, Some(PermissionWasRefused(this)))
    }
  }

  def reason = status.reason

  def toTuple = status.toTuple
  
  def either:Either[PermRefused, PermApproved] = status match {
    case pa:PermApproved => Right(pa)
    case pr:PermRefused => Left(pr)
  } 

}
