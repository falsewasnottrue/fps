package ch8

import org.scalatest.{FlatSpec, Matchers}
import Gen._
import ch6.SimpleRNG

class GenSpec extends FlatSpec with Matchers {

  val seed = SimpleRNG(23)

  "choose" should "generate values inside range" in {
    val (_, res) = choose(7, 42).sample.run(seed)

    res should be >= 7
    res should be < 42
  }

  "listOfN" should "generate a list of n values" in {
    val (_, res) = listOfN(3, ints).sample.run(seed)

    res.size should be(3)
  }
}
