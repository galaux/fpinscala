package fpinscala.errorhandling

import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {

  "An Option" should "provide a map function" in {
    Some(3).map {_ + 1} should be (Some(4))
  }

  it should "provide a getOrElse function" in {
    Some(3).getOrElse(8) should be (3)
    None.getOrElse(8) should be (8)
  }

  it should "provide a flatMap function" in {
    Some(3).flatMap(a => if (a % 2 == 0) Some(a) else None) should be (None)
    Some(4).flatMap(a => if (a % 2 == 0) Some(a) else None) should be (Some(4))
  }

  it should "provide a orElse function" in {
    Some(3).orElse(Some(0)) should be (Some(3))
    None.orElse(Some(0)) should be (Some(0))
  }

  it should "provide a filter function" in {
    Some(3).filter(_ % 2 == 0) should be (None)
  }

  it should "provide a variance function" in {
    Option.variance(Seq(3.0, 8.0, 5.0, 4.0)) should be(Some(3.5))
    Option.variance(Seq.empty) should be (None)
  }

  it should "provide a map2 function" in {
    Option.map2(Some(2), Some(3))(_ + _) should be (Some(5))
  }

  it should "provide a sequence function" in {
    Option.sequence(List(Some(2), Some(3), Some(5))) should be (Some(List(2, 3, 5)))
    Option.sequence(List(Some(2), Some(3), None, Some(5))) should be (None)
  }

  it should "provide a traverse function" in {
    def even(i: Int) = if (i % 2 == 0) Some(i) else None
    Option.traverse(List(2, 4, 6))(even) should be (Some(List(2, 4, 6)))
    Option.traverse(List(2, 5, 6))(even) should be (None)
  }

  it should "provide a sequence2 function" in {
    Option.sequence2(List(Some(2), Some(3), Some(5))) should be (Some(List(2, 3, 5)))
    Option.sequence2(List(Some(2), Some(3), None, Some(5))) should be (None)
  }

}
