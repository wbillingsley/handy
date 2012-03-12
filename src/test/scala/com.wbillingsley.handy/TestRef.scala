/**
 *
 */
package com.wbillingsley.handy

import org.junit.{BeforeClass, Test}
import org.junit.Assert._
import scala.collection.mutable



object TestRef {

  type HasId = { var id: Long }

  val objMap = mutable.Map.empty[Long, TestItem]
  
  @BeforeClass def prep {
    
    Ref.resolver = Some(new RefResolver {
      def resolve[T](unresolved:UnresolvedRef[T]) = {
        unresolved match {
          case RefById(clazz, id) if clazz == classOf[TestItem] => {
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
    val item = TestItem(1, "one")

    TestRef.objMap.put(1, item)
    assertEquals(
      "Same item was not returned",
      RefItself(TestItem(1, "one")),
      RefById(classOf[TestItem], 1).fetch
    )

    assertEquals(
      "Non-existent item didn't return RefNone",
      RefNone,
      RefById(classOf[TestItem], 2).fetch
    )

  }
  
}