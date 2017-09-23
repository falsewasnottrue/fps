package ch6

import org.scalatest.{FlatSpec, Matchers}
import RNG._

class RNGSpec extends FlatSpec with Matchers {

  "nonNegativeInt" should "return a non-negative integer" in {
    val seed = new scala.util.Random().nextInt
    val rng = SimpleRNG(seed)

    val (res, _) = nonNegativeInt(rng)
    res should be >= 0
  }

  "double" should "return a double beetween 0 and 1" in {
    val seed = new scala.util.Random().nextInt
    val rng = SimpleRNG(seed)

    val (res, _) = double(rng)
    res should be >= 0.0
    res should be < 1.0
  }
}
