package u03

import org.junit._
import org.junit.Assert._
import Lists._

class ListTest:
  import List._
  import Person._

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val lNil: List[Int] = Nil()
  val lst: List[Int] = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  val persons: List[Person] = Cons (Person.Student("Barry Bassi", 2020),
                              Cons(Person.Teacher("Mirko Viroli", "PPS"),
                              Cons(Person.Teacher("Alessandro Ricci", "PCD"), Nil())))

  @Test def testSum() : Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() : Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() : Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testDrop() : Unit =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l,1))
    assertEquals(Cons(30, Nil()), drop(l,2))
    assertEquals(Nil(), drop(l,-2))
    assertEquals(Nil(), drop(l,5))
    assertEquals(Nil(), drop(l,-1))

  @Test def testAppend() : Unit =
    assertEquals (Cons(10,Cons(20, Cons(30, Cons(40, Nil())))), append(l, Cons(40, Nil())))
    assertEquals (Cons(40, Nil()), append(lNil, Cons(40, Nil())))

  @Test def testFlatMap[A,B]() : Unit =
    assertEquals (Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons (v + 1, Nil()) ))
    assertEquals (Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons (v + 1, Cons(v + 2, Nil()) ) ))

  @Test def testMapViaFlatMap() : Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapViaFlatMap(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapViaFlatMap(l)(_ + ""))

  @Test def testFilterViaFlatMap() : Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filterViaFlatMap(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterViaFlatMap(l)(_ != 20))

  @Test def testMax() : Unit =
    assertEquals(Some(25), maximum(Cons(10, Cons(25, Cons(20, Nil())))) )
    assertEquals(Some(-30), maximum(Cons(-100, Cons(-30, Cons(-200, Nil())))) )
    assertEquals(Some(Int.MinValue), maximum(Nil()) )

  @Test def testSelectCourseOfTeachers() : Unit =
    assertEquals(Cons("PPS", Cons("PCD", Nil())), selectCoursesOfTeachers(persons)) 

  @Test def testFoldLeft () : Unit =
    assertEquals(-16,foldLeft(lst)(0)(_ - _))

  @Test def testFoldRight (): Unit =
    assertEquals(-8,foldRight(lst)(0)(_ - _))