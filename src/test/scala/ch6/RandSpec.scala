package ch6

import org.scalatest.{FlatSpec, Matchers}

class RandSpec extends FlatSpec with Matchers {

  "double" should "return a double beetween 0 and 1" in {
    val seed = SimpleRNG(42)

    val (res, _) = Rand.double(seed)
    res should be >= 0.0
    res should be < 1.0
  }


  "intDouble" should "return an int, double-pain" in {
    val rng = SimpleRNG(42)
    val ((_: Int, d: Double), _) = Rand.intDouble(rng)

    d should be >= 0.0
    d should be < 1.0
  }

}
