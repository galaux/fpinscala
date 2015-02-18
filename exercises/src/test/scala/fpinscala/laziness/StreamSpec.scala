package fpinscala.laziness

import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {

  "A Stream" should "provide a toList function" in {
    Stream(3, 6, 7).toList should be (List(3, 6, 7))
    Stream.empty.toList should be (List())
  }

  it should "provide a take function" in {
    Stream(3, 6, 7).take(2).toList should be (List(3, 6))
    Stream(3, 6, 7).take(0) should be (Stream.empty)
    Stream(3, 6, 7).take(5).toList should be (List(3, 6, 7))
  }

  it should "provide a drop function" in {
    Stream(3, 6, 7).drop(2).toList should be (List(7))
    Stream(3, 6, 7).drop(0).toList should be (List(3, 6, 7))
    Stream(3, 6, 7).drop(5) should be (Stream.empty)
  }

  it should "provide a takeWhile function" in {
    Stream(2, 6, 7).takeWhile(_ % 2 == 0).toList should be (List(2, 6))
    Stream(3, 5, 7).takeWhile(_ % 2 == 0) should be (Stream.empty)
    Stream(4, 6, 8).takeWhile(_ % 2 == 0).toList should be (List(4, 6, 8))
  }

  /* Remember that `forAll` is implemented using `foldRight`. This `foldRight` does not use any
     `||` or `&&` which would stop the recursion. Still, this implementation of `forAll` via
     `foldRight` stops as soon as it encounters a `false` which is the feature we are demonstrating
     here!
     One **cannot** argue that `List` already exhibits this behavior because Scala `List.foldRight`
     is **not** implemented using recursion! The point of *this* exercise is to define a recursive
     method (`Stream.foldRight`) which will go all the way to the bottom of the recursion, and
     to make it use lazy eval in order to show we can avoid going all the way to the bottom of the
     recursion.
   */
  it should "provide a forAll function" in {
    var count = 0
    def isEven(i: Int): Boolean = {count += 1; i % 2 == 0}
    Stream(2, 5, 4, 6, 8).forAll(isEven) should be (false)
    count should be (2)
    count = 0
    Stream(2, 6, 4, 8).forAll(isEven) should be (true)
    count should be (4)
  }

  it should "provide a takeWhileViaFoldRight function" in {
    Stream(2, 6, 7).takeWhileViaFoldRight(_ % 2 == 0).toList should be (List(2, 6))
    Stream(3, 5, 7).takeWhileViaFoldRight(_ % 2 == 0) should be (Stream.empty)
    Stream(4, 6, 8).takeWhileViaFoldRight(_ % 2 == 0).toList should be (List(4, 6, 8))
  }

  it should "provide a headOption function" in {
    Stream(2, 6, 7).headOption should be (Some(2))
    Stream.empty.headOption should be (None)
  }

  it should "provide a map function" in {
    Stream(1, 2, 3).map { _ + 1 }.toList should be (List(2, 3, 4))
    Stream(1, 2, 3).map { _.toString }.toList should be (List("1", "2", "3"))
  }

  it should "provide a filter function" in {
    Stream(1, 2, 3, 4).filter { _ % 2 == 0 }.toList should be (List(2, 4))
  }

  it should "provide an append function" in {
    Stream(1, 2).append(Stream(3, 4)).toList should be (List(1, 2, 3, 4))
  }

  it should "provide a flatMap function" in {
    def oppos(a: Int): Stream[Int] = Stream.cons(a, Stream.cons(-1 * a, Stream.empty[Int]))
    Stream(1, 2, 3).flatMap(oppos).toList should be (List(1, -1, 2, -2, 3, -3))
  }

  it should "provide a ones, constant, from and fibs functions" in {
    Stream.ones.take(5).toList should be (List(1, 1, 1, 1, 1))
    Stream.constant(9).take(5).toList should be (List(9, 9, 9, 9, 9))
    Stream.from(3).take(5).toList should be (List(3, 4, 5, 6, 7))
    Stream.fibs.take(7).toList should be (List(0, 1, 1, 2, 3, 5, 8))
  }

  it should "provide an unfold function" in {
    def f(i: Int) =  if (i < 3) Some((s"v$i", i + 1)) else None
    Stream.unfold(0)(f).toList should be (List("v0", "v1", "v2"))
  }

  it should "provide a onesViaUnfold, constantViaUnfold, fromViaUnfold and fibsViaUnfold functions" in {
    Stream.fibsViaUnfold.take(7).toList should be (List(0, 1, 1, 2, 3, 5, 8))
    Stream.fromViaUnfold(3).take(5).toList should be (List(3, 4, 5, 6, 7))
    Stream.constantViaUnfold(9).take(5).toList should be (List(9, 9, 9, 9, 9))
    Stream.onesViaUnfold.take(5).toList should be (List(1, 1, 1, 1, 1))
  }

  it should "provide a mapViaUnfold function" in {
    Stream(1, 2, 3).mapViaUnfold { _ + 1 }.toList should be (List(2, 3, 4))
    Stream(1, 2, 3).mapViaUnfold { _.toString }.toList should be (List("1", "2", "3"))
  }

  it should "provide a takeViaUnfold function" in {
    Stream(3, 6, 7).takeViaUnfold(2).toList should be (List(3, 6))
    Stream(3, 6, 7).takeViaUnfold(0) should be (Stream.empty)
    Stream(3, 6, 7).takeViaUnfold(5).toList should be (List(3, 6, 7))
  }

  it should "provide a takeWhileViaUnfold function" in {
    Stream(2, 6, 7).takeWhileViaUnfold(_ % 2 == 0).toList should be (List(2, 6))
    Stream(3, 5, 7).takeWhileViaUnfold(_ % 2 == 0) should be (Stream.empty)
    Stream(4, 6, 8).takeWhileViaUnfold(_ % 2 == 0).toList should be (List(4, 6, 8))
  }

  it should "provide a zipWithViaUnfold function" in {
    def f(a: String, b: Int) = s"$a: ${b.toString}"
    Stream("1", "2", "3", "4").zipWithViaUnfold(Stream(4, 5, 6))(f).toList should be (List("1: 4", "2: 5", "3: 6"))
  }

  it should "provide a zipAll via unfold function" in {
    def f(a: String, b: Int) = s"$a: ${b.toString}"
    Stream("1", "2", "3")
      .zipAll(Stream(4, 5, 6, 7)).toList should be (
      List(
        (Some("1"), Some(4)),
        (Some("2"), Some(5)),
        (Some("3"), Some(6)),
        (None     , Some(7))
      )
    )
  }

  it should "provide a startsWith function" in {
    Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 4)) should be (false)
    Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 3)) should be (true)
  }

  it should "provide a tails function" in {
    Stream(1, 2, 3).tails.toList.map {_.toList} should be (List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

}
