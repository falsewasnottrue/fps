package ch6

import org.scalatest.{FlatSpec, Matchers}
import Rand._

class RandSpec extends FlatSpec with Matchers {

  "double" should "return a double beetween 0 and 1" in {
    val seed = SimpleRNG(42)

    val (_, res) = Rand.double(seed)
    res should be >= 0.0
    res should be < 1.0
  }

  "intDouble" should "return an int, double-pain" in {
    val rng = SimpleRNG(42)
    val (_, (_: Int, d: Double)) = Rand.intDouble(rng)

    d should be >= 0.0
    d should be < 1.0
  }

  "ints" should "return random ints" in {
    val rng = SimpleRNG(42)
    val (_, is) = Rand.ints(3)(rng)

    is.size should be(3)
  }

  "flatMap" should "combine two Rands" in {
    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }
    }

    val (rng1, result1) = nonNegativeLessThan(10)(SimpleRNG(47))
    val result2 = nonNegativeLessThan(10)(rng1)._2

    result1 should be >= 0
    result1 should be < 10
    result2 should be >= 0
    result2 should be < 10
    result1 should not be result2
  }

}
