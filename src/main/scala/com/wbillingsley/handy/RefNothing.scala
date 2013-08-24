package com.wbillingsley.handy


/**
 * A reference that has nothing at the end of it, either through being a failed reference or the empty reference
 */
trait RefNothing extends ResolvedRef[Nothing] with ResolvedRefMany[Nothing] {
  
  override def getId[TT >: Nothing, K](implicit g:GetsId[TT, K]) = None
  
  def isEmpty = true
  def toOption = None
  def getId = None
  
  override def fetch = this
  
  def foreach[U](f: Nothing => U) { /* does nothing */ }
  
  def map[B](f: Nothing => B) = this  
  
  def flatMapOne[B](f: Nothing => Ref[B]) = this

  def flatMapMany[B](f: Nothing => RefMany[B]) = this
  
  def isTraversableAgain = true
  
  def toIterator = Iterator.empty
  
  def toStream = Stream.empty
  
  def copyToArray[B >: Nothing](xs:Array[B], start:Int, len:Int) { /* nothing to copy */ }
  
  def exists(p: Nothing => Boolean) = false
  
  def find(p: Nothing => Boolean) = None
  
  def forall(p: Nothing => Boolean) = Iterator.empty.forall(p)
  
  def hasDefiniteSize = true
  
  def seq = Iterator.empty.seq
  
  def toTraversable = Seq.empty  
     
  def withFilter(p: Nothing => Boolean) = this  
  
  def first = this
    
  override def toRefOne = this
}


/**
 * A failure to find a reference
 * @param msg description of the failure
 * @param exception an exception if there was one
 */
case class RefFailed(exception: Throwable) extends RefNothing {

  override def toEither = Left(exception)
       
  def onComplete[U](onSuccess: Nothing => U, onNone: => U, onFail: Throwable => U) { 
    onFail(exception)
  } 
  
  def fold[B](initial: =>B)(each: (B, Nothing) => B) = this
  
  def onReady[U](onSuccess: RefMany[Nothing] => U, onNone: => U, onFail: Throwable => U) { onFail(exception) }  
}

/**
 * Singleton to say there's nothing there.
 */
case object RefNone extends RefNothing {
  
  override def toEither = Right(None)  
  
  def onComplete[U](onSuccess: Nothing => U, onNone: => U, onFail: Throwable => U) { 
    onNone
  }
 
  def fold[B](initial: =>B)(each: (B, Nothing) => B) = RefItself(initial)
  
  def onReady[U](onSuccess: RefMany[Nothing] => U, onNone: => U, onFail: Throwable => U) { onNone }
}
