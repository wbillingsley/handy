/**
 *
 */
package com.wbillingsley.handy

import org.junit.{BeforeClass, Test}
import org.junit.Assert._
import scala.collection.mutable


case class TItem(id: Long, name: String)

case object TItem {
  def ref(id:Long) = RefById(classOf[TItem], id)
}

object TestRef {

  type HasId = { var id: Long }

  val objMap = mutable.Map.empty[Long, TItem]
  
  @BeforeClass def prep {
    
    RefById.lookUpMethod = new RefById.LookUp {
      def lookup[T](unresolved: RefById[T, _]) = {
        unresolved match {
          case RefById(clazz, id) if clazz == classOf[TItem] => {
            objMap.get(id.toString.toLong) match {
              case Some(x) => RefItself(x).asInstanceOf[RefItself[T]]
              case _ => RefNone
            }
          }
          case _ => RefNone
        }        
      }
    }

  }
}


class TestRef {

  import TestRef._

  @Test def putAndGet {
    val item = TItem(1, "one")

    // Put an item in the database
    objMap.put(1, item)
    
    // Check we can get it
    assertEquals(
      "Same item was not returned",
      RefItself(TItem(1, "one")),
      RefById(classOf[TItem], 1).fetch
    )

    // Check we can't fet something we didn't put in there
    assertEquals(
      "Non-existent item didn't return RefNone",
      RefNone,
      RefById(classOf[TItem], 2).fetch
    )
  }
  
  @Test def simpleFor {
    val item = TItem(1, "one")

    // Put an item in the database
    objMap.put(1, item)
    
    // Get a reference to just the item's name
    val result = for (i <- TItem.ref(1)) yield i.name
    
    
  }
  
}