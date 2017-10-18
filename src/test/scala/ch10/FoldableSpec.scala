package ch10

import ch3.{Branch, Leaf}
import org.scalatest.{FlatSpec, Matchers}

class FoldableSpec extends FlatSpec with Matchers {

  "Foldable[Tree]" should "implement foldLeft" in {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    TreeFoldable.foldLeft(t)(0)(_ + _) should be(6)
  }

  it should "implement foldRight" in {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    TreeFoldable.foldRight(t)(0)(_ + _) should be(6)
  }

  it should "implement foldMap" in {
    val t = Branch(Branch(Leaf("a"), Leaf("bc")), Leaf("def"))
    TreeFoldable.foldMap(t)(_.length)(Monoid.intAddition) should be(6)
  }
}
