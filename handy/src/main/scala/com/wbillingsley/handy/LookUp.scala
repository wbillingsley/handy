package com.wbillingsley.handy

/** A lookup for one item, requiring it to be present */
type EagerLookUpOne[C, T] = C => Ref[T] // Task[T]

/** A lookup for one item, requiring it to be present */
type LazyLookUpOne[C, T] = C => Task[T]

/** A lookup for one item, which might not exist */
type EagerLookUpOpt[C, T] = C => RefOpt[T]

/** A lookup for one item, which might not exist */
type LazyLookUpOpt[C, T] = C => TaskOpt[T]

type EagerLookUpMany[C, T] = C => RefMany[T]

trait LookUp[-C, +T] {

  def eagerOne(id:C): Ref[T]

  def eagerOpt(id:C): RefOpt[T]
  
  def lazyOne(id:C):Task[T] = Task.prepare(eagerOne(id)) 
  
  def lazyOpt(id:C):TaskOpt[T] = Task.prepareOpt(eagerOpt(id))
  
}

object LookUp {

  

}

