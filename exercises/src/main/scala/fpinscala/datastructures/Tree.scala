package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => math.max(max(l), max(r))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => math.max(1 + depth(l), 1 + depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) =>
      g(
        fold[A, B](l)(f)(g),
        fold[A, B](r)(f)(g)
      )
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1) { (a, b) => a + b + 1 }
  def maxViaFold(t: Tree[Int]): Int = fold(t)(v => v) { (a, b) => math.max(a, b) }
  def depthViaFold[A](t: Tree[A]): Int = fold(t)(v => 0) { (a, b) => math.max(1 + a, 1 + b) }
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((a, b) => Branch(a, b))

}
