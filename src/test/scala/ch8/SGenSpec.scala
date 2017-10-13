package ch8

import org.scalatest.{FlatSpec, Matchers}
import Gen.ints
import ch6._

class SGenSpec extends FlatSpec with Matchers {

  val RNG = SimpleRNG(42)

  "listOf" should "generate different sized lists" in {
    val n = 42

    val ls: SGen[List[Int]] = Gen.listOf(ints)
    val lsOfN: Gen[List[Int]] = ls.forSize(n)
    val (_, sample) = lsOfN.sample.run(RNG)

    sample.size should be(n)
  }

  "map" should "nap to different range" in {
    val s = ints.unsized
    val bools: SGen[Boolean] = s.map(_ % 2 == 0)

    val (_, sample: Boolean) = bools.forSize(1).sample.run(RNG)

    sample should be(false)
  }
}
