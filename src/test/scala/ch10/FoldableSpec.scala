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

  "Foldable[Option]" should "implement foldLeft" in {
    OptionFoldable.foldLeft[Int,Int](None)(1)(_ + _) should be(1)
    OptionFoldable.foldLeft(Some(2))(1)(_ + _) should be(3)
  }

  it should "implement foldRight" in {
    OptionFoldable.foldRight[Int,Int](None)(1)(_ + _) should be(1)
    OptionFoldable.foldRight(Some(2))(1)(_ + _) should be(3)
  }

  it should "implement foldMap" in {
    OptionFoldable.foldMap[Int,Int](None)(identity[Int])(Monoid.intAddition) should be(0)
    OptionFoldable.foldMap(Some(2))(identity[Int])(Monoid.intAddition) should be(2)
  }

  "toList" should "turn an option into a list" in {
    OptionFoldable.toList(None) should be(Nil)
    OptionFoldable.toList(Some(1)) should be(List(1))
  }

  it should "turn a tree into a list" in {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    TreeFoldable.toList(t) should be(List(3,2,1))
  }
}
