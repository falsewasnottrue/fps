package ch8

import org.scalatest.{FlatSpec, Matchers}
import Gen._
import ch6.SimpleRNG

class GenSpec extends FlatSpec with Matchers {

  "choose" should "generate values inside range" in {
    val seed = SimpleRNG(23)

    val (_, res) = choose(7, 42).run(seed)
    res should be >= 7
    res should be < 42
  }

}
