package com.wbillingsley.handy

/**
 * A run-time registry for LookUps. 
 * 
 * Sometimes, applications will want a plug-mechanism whereby functionality can register itself at start-up rather than
 * compiling the connections between components. This enables that -- the code can instantiate a catalog, and import it
 * to implicitly generate LookUps that use the catalog.
 * 
 * If there is no suitable LookUp in the catalog when a RefById is looked up, it will produce a RefFailed with a message
 * saying that no suitable LookUp had been registered.
 * 
 * Thus, using a catalog defers the check for whether a suitable LookUp exists to run-time (specifically, when a RefById is looked up)
 * rather than compile-time (specifically, when the code creating the RefById is compiled)
 */
class LookUpCatalog {
  
  /*
   * A map of (Class[T], Class[K]) -> Lookup[T, K] for any [T, K]. The constraint is maintained by the registration method
   */
  private val catalog = scala.collection.mutable.Map.empty[(Class[_], Class[_]), LookUp[_, _]]
  
  def registerLookUp[C, T](keyClass: Class[C], resultClass: Class[T], lookup:LookUp[C, T]) = this.synchronized {
    catalog.put((keyClass, resultClass), lookup)
  }
  
  /**
   * A specialised lookup that uses the catalog. 
   * 
   * This is a case class, so that two GeneratedLookups will be equal if they are in the same catalog and lookup the same class.
   */
  case class GeneratedLookup[C, T](keyClass: Class[C], lookupClass: Class[T]) extends LookUp[C, T] {
    
    def catalogLookUp:Ref[LookUp[C, T]] = catalog.get((keyClass, lookupClass)) match {
      case Some(l: LookUp[_, _]) => RefItself(l.asInstanceOf[LookUp[C, T]])
      case _ => RefFailed(new IllegalArgumentException(s"I don't know how to look up a ${lookupClass.getName} with key ${keyClass.getName}"))
    }
    
    def eagerOne(id: C):Ref[T] = catalogLookUp.flatMapOne(_.eagerOne(id))

    def eagerOpt(id: C): RefOpt[T] = catalogLookUp.flatMapOpt(_.eagerOpt(id))
    
  }
  
  /**
   * Generates a specialised lookup that uses the catalog
   */
  def genLookUp[C, T](keyClass: Class[C], lookupClass: Class[T]):LookUp[C, T] = new GeneratedLookup[C, T](keyClass, lookupClass)

}