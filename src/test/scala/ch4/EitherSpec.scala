package ch4

import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {

  "map" should "return right value from right value" in {
    Right(1).map(_ + 1) should be(Right(2))
  }

  it should "return left value from left value" in {
    val v: Either[String, Int] = Left("error")
    v.map(_ + 1) should be(Left("error"))
  }
}
