package u03

import scala.annotation.tailrec

object ListsExercises2 extends App{
  import List.*
  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    @annotation.tailrec
    def drop[A] (l : List[A], nElems: Int): List[A] = (l, nElems) match
      case (Cons(h,t), nElems) if nElems == 0 => l
      case (Cons(_,t), nElems) => drop (t, nElems-1)
      case _ => Nil()

    def append[A] (l: List[A], tail: List[A]): List[A] = l match
      case Cons(h, t) => Cons(h, append (t, tail))
      case Nil() => tail


    //Book excercises ("Functional programming with Scala")
    def tail[A](l: List[A]): List[A] = l match
      case Cons (_,t) => t
      case Nil() => Nil()

    def setHead[A](l: List[A], h: A): List[A] = l match
      case Cons (_,t) => Cons (h, t)
      case Nil() => Nil()

    //def dropWhile[A] (l : List[A], nElems: Int): List[A] = (l, nElems) match
    def dropWhile[A] (l : List[A], pred: A => Boolean): List[A] = l match
      case Cons(h, t) if !pred(h) => Cons(h, dropWhile(t, pred))
      case Cons(_, t) => dropWhile(t, pred)
      case Nil() => Nil()

    //def dropWhile[A] (l : List[A], nElems: Int): List[A] = (l, nElems) match
    def dropWhileCurried[A] (l : List[A])(pred: A => Boolean): List[A] = l match
      case Cons(h, t) if !pred(h) => Cons(h, dropWhile(t, pred))
      case Cons(_, t) => dropWhile(t, pred)
      case Nil() => Nil()

    def init[A](l:List[A]): List[A] = l match
      case Cons(h, Nil()) => Nil()
      case Cons(h, t) => Cons(h, init(t))
      case _ => Nil()

    def foldRight [A,B](l: List[A], z:B)(f: (A,B) => B) : B = l match
        case Nil() => z
        case Cons(h, t) => f(h, foldRight(t,z)(f))

    def sum2(l: List[Int]) :Int=
      foldRight(l, 0)((x,y) => x + y)

    def product2(l: List[Double]) :Double=
      foldRight(l, 1.0)(_ * _)

    def length[A](l: List[A]): Int = l match
      case Nil() => 0
      case Cons(h, t) => length(t)+1

    def lengthFR[A](l: List[A]): Int =
      foldRight(l, 0)((_,acc) => acc+1)

    //Ex 3.10
    @tailrec
    def foldLeft[A,B](l: List[A], z:B)(f: (B,A) => B):B = l match
      case Nil() => z
      case Cons(h, t) => foldLeft(t, f(z,h))(f)

    //Ex 3.12
    def lengthFL[A](l: List[A]): Int =
      foldLeft(l, 0)((acc,_) => acc+1)

    //Ex 3.12
    def sumFL(l: List[Int]) : Int =
      foldLeft(l, 0)(_ + _)

    //Ex 3.12
    def productFL(l: List[Double]) : Double =
      foldLeft(l, 1.0)(_ * _)

    def reverse[A](l: List[A]) : List[A] = ???
      //foldLeft(l, Cons(_,_)) ((acc,h) => Cons(h,acc))
      //foldLeft(l, Cons(_,_)) ((acc:,h) => Cons(h,acc))

  /*
    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()
*/
  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
}
