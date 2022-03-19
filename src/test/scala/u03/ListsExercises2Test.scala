package u03

import org.junit.*
import org.junit.Assert.*
import ListsExercises2.*

class ListsExercises2Test {
   import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val ld: List[Double] = Cons(10, Cons(0, Cons(30, Nil())))
  val lNil: List[Int] = Nil()


  @Test def testTail[A]() =
    assertEquals(Cons(20, Cons(30, Nil())), tail(l))
    assertEquals(Nil(), tail(Nil()));

  @Test def testHead[A]() =
    assertEquals(Cons(15, Cons(20, Cons(30, Nil()))), setHead(l,15))
    assertEquals(Nil(), Nil())

  @Test def dropWhileTest[A] =
    assertEquals(Cons(10, Cons(30, Nil())), dropWhile(l,_ == 20 ))
    assertEquals(Cons(10, Nil()), dropWhile(l, _>= 20))

  @Test def dropWhileCurriedTest[A] =
    assertEquals(Cons(10, Cons(30, Nil())), dropWhileCurried(l)(_ == 20 ))
    assertEquals(Cons(10, Nil()), dropWhile(l, _>= 20))

  @Test def initTest[A] =
    assertEquals(Cons(10, Cons(20, Nil() )), init(l))

  @Test def sum2Test =
    assertEquals(60, sum2(l))

  @Test def product2Test =
    assertEquals(6000.0, product2(ld),0)

  @Test def testFoldRightNil =
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), foldRight(l, Nil():List[Int])(Cons(_,_)))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), foldRight(l, Nil():List[Int])(Cons(_,_)))

  @Test def testLength =
    assertEquals(3, length(l))

  @Test def testLengthFR =
    assertEquals(3, lengthFR(l))

  @Test def testLengthFL =
    assertEquals(3, lengthFL(l))

  @Test def sumFLTest =
    assertEquals(60, sumFL(l))

  @Test def productFLTest =
    assertEquals(6000.0, productFL(ld),0)

  //@Test def reverse =
  //  assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(l))
}
