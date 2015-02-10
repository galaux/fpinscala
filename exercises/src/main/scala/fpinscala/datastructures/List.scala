package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
      case Nil => throw new Exception("Nil has no tail")
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
  l match {
    case Nil => throw new Exception("Nil has no head to replace")
    case _ => Cons(h, tail(l))
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
        case Nil => throw new Exception("Not enough elements to drop")
        case Cons(_, t) => drop(t, n - 1)
      }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) { dropWhile(t, f) } else l
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Nil has no init")
    case Cons(a, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, elem) => elem + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, xs) => foldLeft(xs, f(z, h))(f)
    }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((a, b) => a + 1)

  // Initial try was the following. But the compiler infers that the type of 'f' is (Nil, List[A]) => List[A]
//def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil)      ((l1, e) => Cons(e, l1))
//def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil)      ((l1, e) => Cons(e, l1))
  def reverse[A](l: List[A]): List[A] = foldLeft            (l, List[A]())((l1, e) => Cons(e, l1))

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, List[A]())(append2)

  def addOne(l: List[Int]): List[Int] = foldRight(l, List[Int]())((a, b) => Cons(a + 1, b))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def filterUnless[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A]) {
      (a, b) => if (f(a)) Cons(a, b) else b
    }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterUnless2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def addPairWise(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
    }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] =
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith[A, B, C](t1, t2)(f))
    }

  //def hasSubSequence[A](main: List[A], sub: List[A]): Boolean = {
  //  (main, sub) match {
  //    case (_, Nil) => true
  //    case (Nil, _) => false
  //    case (Cons(mainH, mainT), Cons(subH, subT)) if (mainH == subH) => hasSubSequence(mainT, subT)
  //  }
  //}

}
