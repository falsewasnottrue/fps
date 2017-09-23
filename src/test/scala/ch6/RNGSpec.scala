package ch6

import org.scalatest.{FlatSpec, Matchers}

class RNGSpec extends FlatSpec with Matchers {

  "nonNegativeInt" should "return a non-negative integer" in {
    val seed = new scala.util.Random().nextInt
    val rng = SimpleRNG(seed)

    val (res, _) = rng.nonNegativeInt(rng)
    res should be >= 0
  }
}
