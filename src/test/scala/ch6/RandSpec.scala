package ch6

import org.scalatest.{FlatSpec, Matchers}

class RandSpec extends FlatSpec with Matchers {

  "double" should "return a double beetween 0 and 1" in {
    val seed = SimpleRNG(42)

    val (res, _) = Rand.double(seed)
    res should be >= 0.0
    res should be < 1.0
  }

}
