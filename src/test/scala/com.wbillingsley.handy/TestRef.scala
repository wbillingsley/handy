/**
 *
 */
package com.wbillingsley.handy

import org.junit.{BeforeClass, Test}
import org.junit.Assert._
import scala.collection.mutable


case class TItem(id: Long, name: String)

object TestRef {

  type HasId = { var id: Long }

  val objMap = mutable.Map.empty[Long, TItem]
  
  @BeforeClass def prep {
    
    Ref.resolver = Some(new RefResolver {
      def resolve[T](unresolved:UnresolvedRef[T]) = {
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
    })

  }
}


class TestRef {


  @Test def putAndGet {
    val item = TItem(1, "one")

    TestRef.objMap.put(1, item)
    assertEquals(
      "Same item was not returned",
      RefItself(TItem(1, "one")),
      RefById(classOf[TItem], 1).fetch
    )

    assertEquals(
      "Non-existent item didn't return RefNone",
      RefNone,
      RefById(classOf[TItem], 2).fetch
    )

  }
  
}