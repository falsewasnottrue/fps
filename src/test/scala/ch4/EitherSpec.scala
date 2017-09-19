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

  "flatMap" should "work on right value" in {
    Right(1).flatMap(i => Right(i+1)) should be(Right(2))
    Right(1).flatMap(_ => Left("error")) should be(Left("error"))
  }

  it should "work on left value" in {
    val v: Either[String, Int] = Left("error")
    v.flatMap(i => Right(i+1)) should be(v)
    v.flatMap(_ => Left("error 2")) should be(Left("error"))
  }
}
