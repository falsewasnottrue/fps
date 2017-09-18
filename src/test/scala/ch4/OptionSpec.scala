package ch4

import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  "map" should "map value if it is present" in {
    (None: Option[Int]).map(_ + 1) should be(None)
    Some(1).map(_ + 1) should be(Some(2))
  }

  "flatMap" should "flatMap value if it is present" in {
    Some(1).flatMap(i => Some(i * 3)) should be(Some(3))
    Some(1).flatMap(i => None) should be(None)
  }

  it should "return None if there is no value" in {
    (None: Option[Int]).flatMap(_ => Some(1)) should be(None)
    (None: Option[Int]).flatMap(_ => None) should be(None)
  }

  "getOrElse" should "return the value it is present" in {
    Some(1).getOrElse(2) should be(1)
  }

  it should "return the default if there is no value" in {
    None.getOrElse(2) should be(2)
  }

  // orElse
  // filter

}
