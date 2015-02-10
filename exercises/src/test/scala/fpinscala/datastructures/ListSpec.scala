package fpinscala.datastructures

import org.scalatest._

class ListSpec extends FlatSpec with Matchers {

  "A List" should "provide a tail function" in {
    List.tail(List(1, 2, 3)) should be (List(2, 3))
    a [Exception] should be thrownBy { List.tail(Nil) }
  }

  it should "provide a setHead function" in {
    List.setHead(List(1, 2, 3), 4) should be (List(4, 2, 3))
    a [Exception] should be thrownBy { List.setHead(Nil, 3) }
  }

  it should "provide a drop function" in {
    a [Exception] should be thrownBy { List.drop(Nil, 3) }
    a [Exception] should be thrownBy { List.drop(List(1, 2), 3) }
    List.drop(List(1, 2, 3), 2) should be (List(3))
  }

  it should "provide a dropWhile function" in {
    List.dropWhile(Nil, (i: Int) => i <= 0) should be (Nil)
    List.dropWhile(List(-1, -2, 3), (i: Int) => i <= 0) should be (List(3))
    List.dropWhile(List(1, 2, 3), (i: Int) => i <= 0) should be (List(1, 2, 3))
    List.dropWhile(List(-1, -2, -3), (i: Int) => i <= 0) should be (Nil)
  }

  it should "provide a init function" in {
    a [Exception] should be thrownBy { List.init(Nil) }
    List.init(List(3)) should be (Nil)
    List.init(List(1, 2, 3)) should be (List(1, 2))
  }

  it should "show result of passing Nil and Cons themselves to foldRight" in {
    println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_,_)))
  }

  it should "provide a length function" in {
    List.length[Int](List(1, 2, 3, 4, 5)) should be (5)
  }

  it should "provide a tail recursive foldLeft function" in {
    List.foldLeft(List(1, 2, 3), 0)(_ + _) should be (6)
  }

  it should "provide a sum function with foldLeft" in {
    List.sum3(List(1, 2, 3)) should be (6)
  }

  it should "provide a product function with foldLeft" in {
    List.product3(List(1, 2, 3, 4)) should be (24)
  }

  it should "provide a length function with foldLeft" in {
    List.length2(List(3, 9, 1, 3, 4)) should be (5)
  }

  it should "provide a reverse function with fold" in {
    List.reverse(List(1, 2, 3, 4)) should be (List(4, 3, 2, 1))
  }

  it should "provide a append function with fold" in {
    List.append2(List(1, 2), List(3, 4, 5)) should be (List(1, 2, 3, 4, 5))
  }

  it should "provide a concat function with fold" in {
    List.concat(List(List(1, 2), List(3, 4, 5))) should be (List(1, 2, 3, 4, 5))
  }

  it should "provide a addOne function" in {
    List.addOne(List(1, 3, 4)) should be (List(2, 4, 5))
  }

  it should "provide a doubleToString function" in {
    List.doubleToString(List(1.3, 3.9, 4.3)) should be (List("1.3", "3.9", "4.3"))
  }

  it should "provide a map function" in {
    List.map(List(1, 3, 4))(_ * -1) should be (List(-1, -3, -4))
  }

  it should "provide a filter function" in {
    List.filterUnless(List[Int](1, 3, 4, 2, 5))(_ % 2 == 0) should be (List(4, 2))
  }

  it should "provide a flatMap function" in {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) should be (List(1, 1, 2, 2, 3, 3))
  }

  it should "provide a filter function that use flatMap" in {
    List.filterUnless2(List[Int](1, 3, 4, 2, 5))(_ % 2 == 0) should be (List(4, 2))
  }

  it should "provide a addPairWise function" in {
    List.addPairWise(List(1, 2, 3), List(4, 5, 6)) should be (List(5, 7, 9))
  }

  it should "provide a zipWith function" in {
    List.zipWith(List("1", "2", "3"), List(4, 5, 6))((a, b) => s"$a: ${b.toString}") should be (List("1: 4", "2: 5", "3: 6"))
  }

//  it should "provide a hasSubSequence function" in {
//    List.hasSubSequence(List(1, 2, 3, 4), List(2, 3)) should be (true)
//    List.hasSubSequence(List(1, 2, 3, 4), List(3, 4)) should be (true)
//    List.hasSubSequence(List(1, 2, 3, 4), List(1, 2, 3)) should be (true)
//    List.hasSubSequence(List(1, 2, 3, 4), List(1, 2, 3, 4)) should be (true)
//    List.hasSubSequence(List(1, 2, 3, 4), List(1, 3, 4)) should be (false)
//  }

}
