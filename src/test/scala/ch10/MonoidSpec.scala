package ch10

import org.scalatest.{FlatSpec, Matchers}
import Monoid._
import MonoidLaws._
import ch8.{Gen, Passed, Prop}

class MonoidSpec extends FlatSpec with Matchers {

  "intAddition" should "meet the monoid laws" in {
    Prop.run(laws(intAddition)(Gen.ints)) should be(Passed)
  }

  "intMultiplication" should "meet the monoid laws" in {
    Prop.run(laws(intMultiplication)(Gen.ints)) should be(Passed)
  }

  "optionMonoid" should "meet the monoid laws" in {
    Prop.run(laws(optionMonoid[Int])(Gen.opts(Gen.ints))) should be(Passed)
  }
}
