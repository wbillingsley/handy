package com.wbillingsley.handy

case class Ids[T, K](ids: Seq[K]) {

  def lookUp(implicit lu: LookUp[T,K]) = lu.many(this)

  def contains(id:Id[T, K]) = ids.contains(id.id)

  def toSeqId:Seq[Id[T, K]] = ids.map(new Id(_))

  def toRefMany(implicit lu:LookUp[T, K]) = RefManyById(ids)(lu)

}

case class UntypedIds[K](id: K) {
  def of[T] = new Id[T,K](id)
}

object Ids {
  def apply[K](ids:Seq[K]) = UntypedIds(ids)

  def empty[T, K] = Ids[T,K](Seq.empty)

  type LookUpMany[T,K] = Ids[T,K] => RefMany[T]

  case class LookUpPair[T,K](ids:Ids[T,K], lu:LookUpMany[T,K])

  implicit class AsIds[K](val k:Seq[K]) extends AnyVal {
    def asIds[T] = Ids[T,K](k)
  }

  implicit class ManyIdToIds[T, K](val m:RefMany[Id[T,K]]) extends AnyVal {
    def toIds:Ref[Ids[T,K]] = for {
      singleIds <- m.toRefOne
      seq = singleIds.toSeq
      ids = seq.map(_.id).asIds[T]
    } yield ids
  }
}

