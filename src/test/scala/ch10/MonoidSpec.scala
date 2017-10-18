package ch10

import org.scalatest.{FlatSpec, Matchers}
import Monoid._
import MonoidLaws._
import ch8.{Gen, Passed, Prop}

class MonoidSpec extends FlatSpec with Matchers {

  "listMonoid" should "meet the monoid laws" in {
    Prop.run(laws(listMonoid[Int])(Gen.listOf(Gen.ints).forSize(10))) should be(Passed)
  }

  "intAddition" should "meet the monoid laws" in {
    Prop.run(laws(intAddition)(Gen.ints)) should be(Passed)
  }

  "intMultiplication" should "meet the monoid laws" in {
    Prop.run(laws(intMultiplication)(Gen.ints)) should be(Passed)
  }

  "booleanOr" should "meet the monoid laws" in {
    Prop.run(laws(booleanOr)(Gen.bools)) should be(Passed)
  }

  "booleanAnd" should "meet the monoid laws" in {
    Prop.run(laws(booleanAnd)(Gen.bools)) should be(Passed)
  }

  "optionMonoid" should "meet the monoid laws" in {
    Prop.run(laws(optionMonoid[Int])(Gen.opts(Gen.ints))) should be(Passed)
  }

  "foldMap" should "fold a list using a monoid" in {
    foldMap(List(1,2,3,4), intAddition)(identity[Int]) should be(10)
    foldMap(List(1,2,3,4), intMultiplication)(identity[Int]) should be(24)
  }
}
