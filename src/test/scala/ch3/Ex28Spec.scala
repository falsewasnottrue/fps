package ch3

import org.scalatest.{FlatSpec, Matchers}
import Tree._

class Ex28Spec extends FlatSpec with Matchers {

  "map" should "map to same type" in {
    map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_+1) should be(Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
  }

  it should "map to another type" in {
    map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_.toString) should be(Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3"))))
  }
}
