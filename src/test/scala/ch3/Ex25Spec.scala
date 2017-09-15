package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex25Spec extends FlatSpec with Matchers {

  "size" should "calculate the size of a tree" in {
    Tree.size(Leaf(1)) should be(1)
    Tree.size(Branch(Leaf(1), Leaf(2))) should be(3)

    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.size(t) should be(5)
  }
}
