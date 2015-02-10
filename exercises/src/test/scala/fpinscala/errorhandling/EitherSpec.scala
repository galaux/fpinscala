package fpinscala.errorhandling

import org.scalatest._

class EitherSpec extends FlatSpec with Matchers {

  def even(i: Int) = if (i % 2 == 0) Right(i) else Left("not even")

  "An Either" should "provide a map method" in {
    Right("test").map { str => s"Good result is: '$str'" } should be (Right("Good result is: 'test'"))
    Left("test").map { str => s"Good result is: '$str'" } should be (Left("test"))
  }

  it should "provide a flatMap method" in {
    Right(4).flatMap(even) should be (Right(4))
    Right(3).flatMap(even) should be (Left("not even"))
    Left("there was a problem").flatMap(even) should be (Left("there was a problem"))
  }

  it should "provide a orElse method" in {
    Right(4).orElse(Right("should NOT be used")) should be (Right(4))
    Left(4).orElse(Right("SHOULD be used")) should be (Right("SHOULD be used"))
  }

  it should "provide a map2 method" in {
    Right(4).map2(Right(3))(_ + _) should be (Right(7))
    val left: Either[String, Int] = Left("initial problem")
    left.map2(Right(4))(_ + _) should be (Left("initial problem"))
    Right(3).map2(left)(_ + _) should be (Left("initial problem"))
  }

  it should "provide a sequence method" in {
    Either.sequence(List(Right(3), Right(2), Right(1))) should be (Right(List(3, 2, 1)))
    Either.sequence(List(Right(3), Left("error"), Right(1))) should be (Left("error"))
  }

  it should "provide a traverse method" in {
    Either.traverse(List(2, 4, 6))(even) should be (Right(List(2, 4, 6)))
    Either.traverse(List(2, 4, 5))(even) should be (Left("not even"))
  }

}
