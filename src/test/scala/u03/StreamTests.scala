package u03

import org.junit._
import org.junit.Assert._
import Lists._
import Streams._

class StreamTests:

  import List._
  import Stream._

  val s = Stream.take(Stream.iterate(0)(_ + 1))(10)

  @Test def testDrop() : Unit =
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))

  @Test def testConstant() : Unit =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), Stream.toList(take(constant("x"))(5)))

  @Test def testFib() : Unit =
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil() )))))))), Stream.toList(Stream.take(fibs)(8)))