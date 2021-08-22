package com.wbillingsley.handy

/**
  * Tests that the type system produces the correct pluralities for Ref. Also test synchronous values
  */
class RefSuite extends munit.FunSuite {
  

  test("RefItself flatMap RefItself produces RefItself") {
    // Only compiles if the type is what we want
    val result:Ref[Int] = RefItself(1).flatMap((x) => RefItself(x))
    
    // Only passes if the value is correct
    assertEquals(result, RefItself(1))
  }
  
  test("RefItself flatMap RefOpt produces RefOpt") {
    // Because we have multiple assertions to check the value, I've combined the type test into the same line
    assertEquals(RefItself(1).flatMap((x) => RefOpt(Some(x))):RefOpt[Int], RefOpt(Some(1)))
    assertEquals(RefItself(1).flatMap((x) => RefNone):RefOpt[Int], RefNone)
  }

  test("RefItself flatMap RefMany produces RefMany") {
    assertEquals(RefItself(1).flatMap((x) => RefIterableOnce(Seq(x, x))):RefMany[Int], RefIterableOnce(Seq(1, 1)))
  }

  test("RefOpt flatMap Ref produces RefOpt") {
    assertEquals(RefOpt(Some(1)).flatMap((x) => RefItself(x)):RefOpt[Int], RefOpt(Some(1)))
    assertEquals(RefNone.flatMap((x) => RefItself(x)):RefOpt[Int], RefNone)
  }
  
  test("RefOpt flatMap RefOpt produces RefOpt") {
    assertEquals(RefOpt(Some(1)).flatMap((x) => RefOpt(Some(x))):RefOpt[Int], RefOpt(Some(1)))
    assertEquals(RefNone.flatMap((x) => RefOpt(Some(x))), RefNone)
    assertEquals(RefOpt(Some(1)).flatMap((x) => RefNone), RefNone)
    assertEquals(RefNone.flatMap((x) => RefNone), RefNone)
  }

  test("RefOpt flatMap RefMany produces RefMany") {
    assertEquals(RefOpt(Some(1)).flatMap((x) => RefIterableOnce(Seq(x, x))), RefIterableOnce(Seq(1, 1)))
    assertEquals(RefNone.flatMap((x) => RefIterableOnce(Seq(x, x))), RefEmpty)
  }

  test("RefMany flatMap Ref produces RefMany") {
    // Test the type
    val result1 = RefIterableOnce(Seq(1, 2)).flatMap((x) => RefItself(x)):RefMany[Int]
    // As the result will be an iterator, it's harder to directly compare. Call collect to convert to a Ref[Seq[Int]]
    assertEquals(result1.collect, RefItself(Seq(1, 2)))

    // We can do the test for RefEmpty in a single line, however
    assertEquals(RefEmpty.flatMap((x) => RefItself(x)):RefMany[Int], RefEmpty)
  }

  test("RefMany flatMap RefOpt produces RefMany") {
    // Test the type
    val result1 = RefIterableOnce(Seq(1, 2)).flatMap((x) => RefOpt(Some(x))):RefMany[Int]
    // As the result will be an iterator, it's harder to directly compare. Call collect to convert to a Ref[Seq[Int]]
    assertEquals(result1.collect, RefItself(Seq(1, 2)))

    // Test the type
    val result2 = RefIterableOnce(Seq(1, 2)).flatMap((x) => RefNone):RefMany[Int]
    // As the result will be an empty iterator, rather than just RefEmpty. Call collect to convert to a Ref[Seq[Int]]
    assertEquals(result2.collect, RefItself(Seq.empty[Int]))

    // We can do the test for RefEmpty in a single line, however
    assertEquals(RefEmpty.flatMap((x) => RefOpt(Some(x))):RefMany[Int], RefEmpty)
  }

  test("RefMany flatMap RefMany produces RefMany") {
    // Test the type
    val result1 = RefIterableOnce(Seq(1, 2)).flatMap((x) => RefIterableOnce(Seq(x, x))):RefMany[Int]
    // As the result will be an iterator, it's harder to directly compare. Call collect to convert to a Ref[Seq[Int]]
    assertEquals(result1.collect, RefItself(Seq(1, 1, 2, 2)))

    // Test the type
    val result2 = RefIterableOnce(Seq(1, 2)).flatMap((x) => RefEmpty):RefMany[Int]
    // As the result will be an iterator, it's harder to directly compare. Call collect to convert to a Ref[Seq[Int]]
    assertEquals(result2.collect, RefItself(Seq.empty[Int]))

    // We can do the test for a RefEmpty on the left in a single line, however
    assertEquals(RefEmpty.flatMap((x) => RefIterableOnce(Seq(x, x))):RefMany[Int], RefEmpty)
  }
  
  test("Ref for notation with Ref produces Ref") {
    val result = for 
      a <- RefItself(1)
      b <- RefItself(a + 1)
    yield b + 1
    
    assertEquals(result, RefItself(3))
  }

  test("Ref for notation with RefOpt produces Ref") {
    val result = for
      a <- RefItself(1)
      b <- RefSome(a + 1)
    yield b + 1
    
    assertEquals(result, RefSome(3))
  }

  test("Ref for notation with filter produces RefOpt") {
    val result = for
      a <- RefItself(1)
      b <- RefItself(a + 1) if b % 2 == 0 
    yield b + 1

    assertEquals(result, RefSome(3))
  }
}
