package com.wbillingsley.handy

/**
 * A reference to an item that has been fetched.
 */
case class RefItself[T](val item: T) extends ResolvedRef[T] {
  
  def toOption = Some(item)
  
  override def toEither = Right(Some(item))
  
  def isEmpty = false
  
  def map[B](f: T => B) = {
    val x = f(item)
    if (x == null) RefNone else RefItself(x)
  } 
  
  def flatMapOne[B](f: T => Ref[B]):Ref[B] = f(item)

  def flatMapMany[B](f: T => RefMany[B]):RefMany[B] = f(item)

  def foreach[U](f: T => U) { f(item) }
  
  override def getId[K](implicit g:GetsId[T, K]) = g.getId(item)

  override def refId[K](implicit g:GetsId[T, K]) = g.getId(item)

  override def immediateId[K](implicit g:GetsId[T, K]) = g.getId(item)

  def isTraversableAgain = true
  
  def toIterator = Iterator(item)
  
  def toStream = Stream(item)
  
  def copyToArray[B >: T](xs:Array[B], start:Int, len:Int) { 
    toIterator.copyToArray(xs, start, len) 
  }
  
  def orIfNone[B >: T](f: => Ref[B]) = this
  
  def recoverWith[B >: T](pf: PartialFunction[Throwable, Ref[B]]) = this
  
  def exists(p: T => Boolean) = p(item)
  
  def find(p: T => Boolean) = if (p(item)) Some(item) else None
  
  def forall(p: T => Boolean) = p(item)
  
  def hasDefiniteSize = true
  
  def seq = toOption
  
  def toTraversable = toOption  
  
  override def fetch = this
    
  def withFilter(p: T => Boolean) = if (p(item)) this else RefNone
 
  def onComplete[U](onSuccess: T => U, onNone: => U, onFail: Throwable => U) { 
    onSuccess(item)
  }  
  
}