package ch6

import org.scalatest.{FlatSpec, Matchers}
import RNG._

class RNGSpec extends FlatSpec with Matchers {

  "nonNegativeInt" should "return a non-negative integer" in {
    val seed = new scala.util.Random().nextInt
    val rng = Simple(seed)

    val (_, res) = nonNegativeInt(rng)
    res should be >= 0
  }

  "double" should "return a double beetween 0 and 1" in {
    val seed = new scala.util.Random().nextInt
    val rng = Simple(seed)

    val (_, res) = double(rng)
    res should be >= 0.0
    res should be < 1.0
  }

  "intDouble" should "return an int, double-pain" in {
    val rng = Simple(42)
    val (_, (i: Int, d: Double)) = intDouble(rng)

    d should be >= 0.0
    d should be < 1.0
  }

  "doubleInt" should "return an double, int-pain" in {
    val rng = Simple(42)
    val (_, (d: Double, i: Int)) = doubleInt(rng)

    d should be >= 0.0
    d should be < 1.0
  }

  "double3" should "return 3 doubles" in {
    val rng = Simple(42)

    val (_, (d1: Double, d2: Double, d3: Double)) = double3(rng)

    d1 should be >= 0.0
    d1 should be < 1.0

    d2 should be >= 0.0
    d2 should be < 1.0

    d3 should be >= 0.0
    d3 should be < 1.0
  }
}
