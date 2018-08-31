package com.wbillingsley.handy

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

/**
  * A Latch is a clearable Future.
  *
  * These can be chained together using a "lazy observer" pattern. Because the value is set by a lazy operation
  * that produces a Future, a dependent Latch only needs to listen for "clear" messages from its parent.
  * Accordingly, when it is cleared it can remove the listener and only re-add it when it is next asked to
  * produce a value.
  *
  * This is useful in situations such as React.js, where you might have cached values (such as the logged in user)
  * that could have many dependent variables, but which dependent variables you want to actually recalculate
  * (rather than just forget) depends on what is currently being showed in the view.
  *
  * Uses synchronization heavily as I most often use it in environments that are effectively single-sequenced.
  * eg, Scala.js where the environment is single-threaded, or web requests which are normally composed using
  * monads and therefore usually operate sequentially-but-asynchronously across a number of threads.
  * (So synchronization usually does not create a bottle-neck)
  *
  * There is also a "globalNotify" function added, specifically for React.js, that can be hooked into the re-render
  * method so that whenever anything
  *
  */
class Latch[T](op: => Future[T], parents:Seq[Latch[_]] = Seq.empty)(implicit ec:ExecutionContext) {

  private val listeners: mutable.Set[Latch.Listener[T]] = mutable.Set.empty

  /**
    * A listener that detects whether the "parent" Latch has cleared (or changed), and clears this one.
    * This should be registered while this Latch has a Future value, but not when it is clear.
    */
  private val parentListener: Latch.Listener[Any] = {
    case _ => clear()
  }

  /**
    * The cached (clearable) future value
    */
  var cached: Option[Future[T]] = None

  def addListener(l: Latch.Listener[T]) = synchronized {
    listeners.add(l)
  }

  def removeListener(l: Latch.Listener[T]) = synchronized {
    listeners.remove(l)
  }

  /**
    * Can be called to manually clear the Latch.
    */
  def clear() = synchronized {
    cached = None
    parents.foreach(_.removeListener(parentListener))
    listeners.foreach {
      _ (None)
    }
    Latch.globalNotify(None)
  }

  /**
    * Can be called to manually fill the Latch with a value.
    * In this case it will not register a listener with any parent (but beware that one might already be in place)
    */
  def fill(v: T) = synchronized {
    cached = Some(Future.successful(v))
    listeners.foreach {
      _ (Some(Success(v)))
    }
    Latch.globalNotify(Some(Success(v)))
  }

  /**
    * Can be called to manually fill the Latch with a value.
    * In this case it will not register a listener with any parent (but beware that one might already be in place)
    */
  def fillFuture(f: Future[T]): Future[T] = synchronized {
    cached = Some(f)
    parents.foreach(_.addListener(parentListener))
    f.onComplete({ t =>
      listeners.foreach {
        _ (Some(t))
      }
      Latch.globalNotify(Some(t))
    })
    f
  }

  /**
    * Can be called to manually fill the Latch with a failed value.
    * In this case it will not register a listener with any parent (but beware that one might already be in place)
    */
  def fail(t: Throwable) = synchronized {
    cached = Some(Future.failed(t))
    listeners.foreach {
      _ (Some(Failure(t)))
    }
    Latch.globalNotify(Some(Failure(t)))
  }

  /**
    * Checks the state of the latch and its contained Future (if any)
    */
  def isCompleted = cached match {
    case Some(f) => f.isCompleted
    case _ => false
  }

  /**
    * Called to get a Future value from the latch, which might already have been completed.
    * This is usually what triggers the computation.
    */
  def request: Future[T] = {
    cached match {
      case Some(x) => x
      case _ => fillFuture(op)
    }
  }

  /**
    * Produces a dependent Latch, that uses "lazy observation" to keep itself up-to-date with this latch.
    */
  def map[B](t: T => B): Latch[B] = {
    new Latch(request.map(t), Seq(this))
  }

  /**
    * Produces a dependent Latch, that uses "lazy observation" to keep itself up-to-date with this latch.
    * Note: Latch uses flatMap to take advantage of Scala's syntactic sugar for monad-like classes, but
    * the transform is a flatMap on the contained Future (ie, T => Future[B] not T => Latch[B]).
    *
    * A flatMap taking T => Latch[B] would be of limited use, as it would not have the "lazy observer"
    * listener set up.
    */
  def flatMap[B](t: T => Future[B]): Latch[B] = {
    new Latch(request.flatMap(t), Seq(this))
  }

  def combine[B, C](b: Latch[B])(op: (T, B) => C) = {
    val l = new Latch({
      for {
        itemA <- request
        itemB <- b.request
      } yield op(itemA, itemB)
    }, Seq(this, b))(this.ec)
    l
  }
}

object Latch {

  type Listener[T] = (Option[Try[T]] => Unit)

  private val listeners:mutable.Set[Latch.Listener[Any]] = mutable.Set.empty

  /**
    * Called whenever any Latch changes state
    */
  def globalNotify(evt:Option[Try[Any]]) = listeners.foreach(_.apply(evt))

  /**
    * Adds a listener function that will be called whenever any Latch changes state. This is useful, for example,
    * for wiring up declarative view re-rendering so that whenever any cached state in the program changes a
    * re-render is called.
    */
  def addGlobalListener(l:Latch.Listener[Any]) = synchronized {
    listeners.add(l)
  }

  def removeGlobalListener(l:Latch.Listener[Any]) = synchronized {
    listeners.remove(l)
  }

  /**
    * Creates a Latch that already has a value
    */
  def immediate[T](op: T)(implicit ec:ExecutionContext):Latch[T] = {
    val p = Promise[T]()
    p.success(op)
    Latch.lazily(p.future)
  }

  /**
    * A lazy latch that will compute its value (asynchonrously) when request is called
    */
  def lazily[T](op: => Future[T])(implicit ec:ExecutionContext):Latch[T] = new Latch(op)

}
