package ch3

import org.scalatest.{FlatSpec, Matchers}
import Tree._

class TreeSpec extends FlatSpec with Matchers {

  val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

  "size" should "calculate the size of a tree" in {
    Tree.size(Leaf(1)) should be(1)
    Tree.size(Branch(Leaf(1), Leaf(2))) should be(3)
    Tree.size(t) should be(5)
  }

  "maximum" should "calculate the maximum in a tree" in {
    maximum(Leaf(1)) should be(1)
    maximum(t) should be(3)
  }

  "depth" should "calculate tree depth" in {
    depth(t) should be(3)
  }

  "map" should "map to same type" in {
    map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_+1) should be(Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
  }

  it should "map to another type" in {
    map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_.toString) should be(Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3"))))
  }

  "fold" should "implement all higher-order functions via fold" in {
    sizeViaFold(t) should be(5)
    maximumViaFold(t) should be(3)
    depthViaFold(t) should be(3)
    mapViaFold(t)(_ % 2 == 0) should be(Branch(Branch(Leaf(false), Leaf(true)), Leaf(false)))
  }
}
