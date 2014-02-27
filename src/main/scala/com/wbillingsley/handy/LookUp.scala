package com.wbillingsley.handy

/**
 * This has to be a trait, rather than just a function because a RefManyById[T, K] is already a RefMany[T].
 * Which means that if it was just a function, then Predef.conforms would be implicitly found.
 *
 * It also has to be invariant in T because two RefByIds are equal if they have the same Id and LookUpMethod
 *
 * It is covariant in K so that a LookUp[Foo, Any] can be used in place of a LookUp[Foo, String]
 */
trait LookUpOne[T, -K] {
  def lookUpOne[KK <: K](r:RefById[T, KK]):Ref[T]
}

object LookUpOne {
  /**
   * A look up that always returns the item you just gave it
   */
  case class AlwaysReturns[T](r:Ref[T]) extends LookUpOne[T, Any] {
    def lookUpOne[KK <: Any](r:RefById[T, KK]):Ref[T] = r
  }
}

/**
 * This has to be a trait, rather than just a function because a RefManyById[T, K] is already a RefMany[T].
 * Which means that if it was just a function, then Predef.conforms would be implicitly found.
 */
trait LookUpMany[T, -K] {
  def lookUpMany[KK <: K](r:RefManyById[T, KK]):RefMany[T]
}

object LookUp {

  def empty[T, K] = new LookUp[T, K] {
    override def lookUpOne[KK <: K](r:RefById[T, KK]) = RefNone

    override def lookUpMany[KK <: K](r:RefManyById[T, KK]) = RefNone
  }

  def fails[T, K](msg:String) = new LookUp[T, K] {
    override def lookUpOne[KK <: K](r:RefById[T, KK]) = new RefFailed(new IllegalStateException(msg))

    override def lookUpMany[KK <: K](r:RefManyById[T, KK]) = new RefFailed(new IllegalStateException(msg))
  }

}

trait LookUp[T, -K] extends LookUpMany[T, K] with LookUpOne[T, K]
