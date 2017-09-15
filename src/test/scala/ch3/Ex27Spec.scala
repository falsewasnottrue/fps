package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex27Spec extends FlatSpec with Matchers {

  "depth" should "calculate tree depth" in {
    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.depth(t) should be(3)
  }
}
