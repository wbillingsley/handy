package com.wbillingsley.handy

/**
 * A run-time registry for LookUps. 
 * 
 * Often, applications will want a plug-mechanism whereby functionality can register itself at start-up rather than
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
  
  def registerLookUp[T, K](lookupClass: Class[T], keyClass: Class[K], lookup:LookUp[T, K]) = this.synchronized {
    catalog.put((lookupClass, keyClass), lookup)
  }
  
  def registerStringLookUp[T](lookupClass: Class[T], lookup:LookUp[T, String]) = registerLookUp(lookupClass, classOf[String], lookup)

  /**
   * A specialised lookup that uses the catalog. 
   * 
   * This is a case class, so that two GeneratedLookups will be equal if they are in the same catalog and lookup the same class.
   */
  case class GeneratedLookup[T, K](val lookupClass: Class[T]) extends LookUp[T, K] {
    def one[KK <: K](id:Id[T, KK]):Ref[T] = {
      val keyClass = id.getClass
      
      catalog.get((lookupClass, keyClass)) match {
        case Some(l:LookUp[_, _]) => l.asInstanceOf[LookUp[T,K]].one(id)
        case _ => RefFailed(new IllegalArgumentException(s"I don't know how to look up a ${lookupClass.getName} with key ${keyClass.getName}"))
      }
    }
    
    def many[KK <: K](ids:Ids[T, KK]):RefMany[T] = {
      val keyHead = ids.ids.headOption
      
      keyHead match {
        case Some(key) => {
          val keyClass = key.getClass
          catalog.get((lookupClass, keyClass)) match {
            
            // Note that asInstanceOf here is true because of the constraint in the registration method
            case Some(l:LookUp[_, _]) => l.asInstanceOf[LookUp[T,K]].many(ids)
            
            case _ => RefFailed(new IllegalArgumentException(s"I don't know how to look up a ${lookupClass.getName} with key ${keyClass.getName}"))
          }
        }
        
        // If there are no keys then there a no referred items 
        case None => RefNone
      }
    }
  }
  
  /**
   * Generates a specialised lookup that uses the catalog
   */
  def genLookUp[T,K](lookupClass: Class[T]):LookUp[T,K] = new GeneratedLookup[T, K](lookupClass)

  /**
   * Creates a LazyId whose LookUp uses the catalog.
   *
   * For example, {@code catalog.lazyId(classOf[Foo], 1)}
   */
  def lazyId[T, K](clazz:Class[T], id:K) = {
    val lu:LookUp[T, K] = genLookUp[T, K](clazz)
    LazyId(id).of(lu)
  }

}