package ch3

import org.scalatest.{FlatSpec, Matchers}
import Tree._

class Ex29Spec extends FlatSpec with Matchers {

  it should "implement all higher-order functions via fold" in {
    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    sizeViaFold(t) should be(5)
    maximumViaFold(t) should be(3)
    depthViaFold(t) should be(3)
    mapViaFold(t)(_ % 2 == 0) should be(Branch(Branch(Leaf(false), Leaf(true)), Leaf(false)))
  }
}
