package u03

object Lists extends App:

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

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h),flatMap(t)(f))
      case Nil() => Nil()

    def mapViaFlatMap[A,B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(a => Cons(mapper(a),Nil()))

    def filterViaFlatMap[A](l: List[A])(pred: A => Boolean): List[A] =
      flatMap(l)(a => if (pred(a)) Cons(a,Nil()) else Nil())


  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
