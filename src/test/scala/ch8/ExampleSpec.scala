package ch8

import org.scalatest.{FlatSpec, Matchers}
import Gen._
import Prop._

class ExampleSpec extends FlatSpec with Matchers {

  "an example" should "work" in {
    val smallInts = Gen.choose(-10, 10)
    val maxProp = forAll(listOf(smallInts)) { ns =>
      if (ns.isEmpty) true else {
        val max = ns.max
        !ns.exists(_ > max)
      }
    }

    Prop.run(maxProp)
  }
}
