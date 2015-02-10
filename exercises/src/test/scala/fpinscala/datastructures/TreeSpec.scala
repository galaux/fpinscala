package fpinscala.datastructures

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {

  "A Tree" should "provide a size function" in {
    Tree.size(Leaf(3)) should be (1)
    Tree.size(Branch(Leaf(3), Leaf(1))) should be (3)
    Tree.size(
      Branch(
        Branch(Leaf(3), Leaf(4)),
        Leaf(1)
      )
    ) should be (5)
  }

  it should "provide a max function" in {
    Tree.max(
      Branch(
        Branch(Leaf(5), Leaf(4)),
        Leaf(1)
      )
    ) should be (5)
  }

  it should "provide a depth function" in {
    Tree.depth(
      Branch(
        Branch(
          Leaf(5),
          Branch(Leaf(4), Leaf(3))),
        Leaf(1)
      )
    ) should be (3)
  }

  it should "provide a map function" in {
    val in =
      Branch(
        Branch(
          Leaf(5),
          Branch(Leaf(4), Leaf(3))),
        Leaf(1)
      )

    val out =
      Branch(
        Branch(
          Leaf(15),
          Branch(Leaf(12), Leaf(9))),
        Leaf(3)
      )

    Tree.map(in) { _ * 3 } should be (out)
  }

  it should "provide a sizeViaFold function" in {
    Tree.sizeViaFold[String](Leaf("3")) should be (1)
    Tree.sizeViaFold[String](Branch(Leaf("3"), Leaf("1"))) should be (3)
    Tree.sizeViaFold[String](
      Branch(
        Branch(Leaf("tset"), Leaf("fjsdk")),
        Leaf("")
      )
    ) should be (5)
  }

  it should "provide a maxViaFold function" in {
    Tree.maxViaFold(
      Branch(
        Branch(Leaf(5), Leaf(4)),
        Leaf(1)
      )
    ) should be (5)
  }

  it should "provide a depthViaFold function" in {
    Tree.depthViaFold[String](
      Branch(
        Branch(
          Leaf("5"),
          Branch(Leaf("4"), Leaf("3"))),
        Leaf("1")
      )
    ) should be (3)
  }

  it should "provide a mapViaFold function" in {
    val in =
      Branch(
        Branch(
          Leaf(5),
          Branch(Leaf(4), Leaf(3))),
        Leaf(1)
      )

    val out =
      Branch(
        Branch(
          Leaf(15),
          Branch(Leaf(12), Leaf(9))),
        Leaf(3)
      )

    Tree.mapViaFold(in) { _ * 3 } should be (out)
  }

}
