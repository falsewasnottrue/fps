package ch4

import org.scalatest.{FlatSpec, Matchers}
import Either._

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

  "orElse" should "keep the right value" in {
    Right(1).orElse(Right(2)) should be(Right(1))
    Right(1).orElse(Left("error")) should be(Right(1))
  }

  it should "override left value" in {
    Left("error").orElse(Right(2)) should be(Right(2))
    Left("error").orElse(Left("error 2")) should be(Left("error 2"))
  }

  "map2" should "call function if both values are right" in {
    Right(1).map2(Right(2))(_ + _) should be(Right(3))
  }

  it should "fail if either value is left" in {
    val v: Either[String, Int] = Left("error")

    v.map2(Right(2))(_ + _) should be(v)
    Right(1).map2(v)(_ + _) should be(v)
    v.map2(v)(_ + _) should be(v)
  }

  "sequence" should "collect all right values" in {
    sequence(List(Right(1), Right(2), Right(3))) should be(Right(List(1,2,3)))
  }

  it should "be left if there is a single left value" in {
    sequence(List(Right(1), Left("error"), Right(3))) should be(Left("error"))
  }

  "traverse" should "collect all right values" in {
    traverse(List(1,2,3))(Right(_)) should be(Right(List(1,2,3)))
  }

  it should "be left if there is a single left value" in {
    traverse(List(1,2,3))(i => if (i%2 ==0) Right(i) else Left("error")) should be(Left("error"))
  }
}
