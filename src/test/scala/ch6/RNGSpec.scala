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

  "intDouble" should "return an int, double-pain" in {
    val rng = SimpleRNG(42)
    val ((i: Int, d: Double), _) = intDouble(rng)

    d should be >= 0.0
    d should be < 1.0
  }

  "doubleInt" should "return an double, int-pain" in {
    val rng = SimpleRNG(42)
    val ((d: Double, i: Int), _) = doubleInt(rng)

    d should be >= 0.0
    d should be < 1.0
  }

  "double3" should "return 3 doubles" in {
    val rng = SimpleRNG(42)

    val ((d1: Double, d2: Double, d3: Double), _) = double3(rng)

    d1 should be >= 0.0
    d1 should be < 1.0

    d2 should be >= 0.0
    d2 should be < 1.0

    d3 should be >= 0.0
    d3 should be < 1.0
  }
}
