package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(hl, tl) => hl() :: tl().toList
  }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Empty => Stream.empty[A]
    case Cons(hl, tl) if (n > 0) => cons(hl(), tl().take(n - 1))
    case Cons(hl, tl)            => Stream.empty[A]
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Stream.empty[A]
    case Cons(hl, tl) if (n > 0) => tl().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(hl, tl) if p(hl()) => cons(hl(), tl().takeWhile(p))
    case _ => Stream.empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) cons(a, b) else Stream.empty[A])

  def headOption: Option[A] = foldRight(Option.empty[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B]) { (a, b) => cons(f(a), b) }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) { (a, b) => if (p(a)) cons(a, b) else b }

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s) { (a, b) => cons(a, b) }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B]) { (a, b) => f(a).append(b) }

  def startsWith[B](s: Stream[B]): Boolean =
    this.zipAll(s).forAll {
      _ match {
        case (Some(a), Some(b)) => a == b
        case (Some(a), None   ) => true
        case _                  => false
      }
    }

  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(l, t) => Some((f(l()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((n, this)) {
      case (ni, Cons(l, t)) if (ni > 0) => Some((l(), (ni - 1, t())))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(l, t) if (p(l())) => Some((l(), t()))
      case _ => None
    }

  def zipWithViaUnfold[B, C](stream: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, stream)) {
      case ( Cons(l1, t1), Cons(l2, t2) ) => Some( ( f(l1(), l2()), (t1(), t2()) ) )
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2)) {
      case ( Cons(l1, t1), Cons(l2, t2) ) => Some( ( (Some(l1()), Some(l2())), (t1() , t2() ) ) )
      case ( Empty       , Cons(l2, t2) ) => Some( ( (None      , Some(l2())), (Empty, t2() ) ) )
      case ( Cons(l1, t1), Empty        ) => Some( ( (Some(l1()), None      ), (t1() , Empty) ) )
      case _ => None
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Cons(l1, t1) => Some( ( cons(l1(), t1()), t1() ) )
      case _ => None
    } append(Stream(Empty))

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
  def fibs: Stream[Int] = {
    def innerFib(a: Int, b: Int): Stream[Int] = Stream.cons(a, innerFib(b, a + b))
    innerFib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((nextValue, nextState)) => cons(nextValue, unfold(nextState)(f))
      case None => Stream.empty[A]
    }

  def fibsViaUnfold: Stream[Int] = unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n) { v => Some(v, v + 1) }
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a) { v => Some(v, v) }
  val onesViaUnfold: Stream[Int] = unfold(1) { _ => Some(1, 1) }

}
