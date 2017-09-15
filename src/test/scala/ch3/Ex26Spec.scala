package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex26Spec extends FlatSpec with Matchers {

  "maximum" should "calculate the maximum in a tree" in {
    Tree.maximum(Leaf(1)) should be(1)

    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.maximum(t) should be(3)
  }
}
