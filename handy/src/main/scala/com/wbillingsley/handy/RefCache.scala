package com.wbillingsley.handy

import scala.collection.mutable
import scala.concurrent.{Promise, ExecutionContext}
import scala.util.Try

/**
  * A cache for Refs.
  *
  * This is a simple class designed for two cases
  * - caches of values within a request on the server
  * - caches of values within a JavaScript context on the client
  *
  * In each case, the cache is not highly parallel -- it is being made for one user and the requests are generally
  * being made sequentially (but possibly asynchronously). And in the JavaScript case, we might want some global
  * action to occur if something is removed or updated in the cache.
  */
class RefCache[K, V]() {

  import java.util.concurrent

  private val cache = new concurrent.ConcurrentHashMap[K, Ref[V]]()

  def get(key:K, f: => Ref[V]):Ref[V] = {
    cache.computeIfAbsent(key, new java.util.function.Function[K, Ref[V]]() {
      override def apply(k:K):Ref[V] = f
    })
  }

  def put(key:K, v: Ref[V]) = synchronized {
    cache.put(key, v)
    for (l <- listeners) l.apply(key, v.toRefOpt)
  }

  def remove(key:K) = synchronized {
    cache.remove(key)
    for (l <- listeners) l.apply(key, RefNone)
  }

  private val listeners:mutable.Set[RefCache.Listener[K, V]] = mutable.Set.empty

  def addListener(l:RefCache.Listener[K, V]) = synchronized {
    listeners.add(l)
  }

  def removeListener(l:RefCache.Listener[K, V]) = synchronized {
    listeners.remove(l)
  }

}

object RefCache {

  type Listener[K, T] = (K, RefOpt[T]) => Unit

  private val listeners:mutable.Set[RefCache.Listener[Any, Any]] = mutable.Set.empty

  /**
    * Called whenever any Cache changes state. Useful in situations such as React.js, where the entire UI state is
    * regenerated on any change of state
    */
  def globalNotify(id:Any, evt:Ref[Any]) = listeners.foreach(_.apply(id, evt.toRefOpt))

  /**
    * Adds a listener function that will be called whenever any Latch changes state. This is useful, for example,
    * for wiring up declarative view re-rendering so that whenever any cached state in the program changes a
    * re-render is called.
    */
  def addGlobalListener(l:RefCache.Listener[Any, Any]) = synchronized {
    listeners.add(l)
  }

  def removeGlobalListener(l:RefCache.Listener[Any, Any]) = synchronized {
    listeners.remove(l)
  }

}