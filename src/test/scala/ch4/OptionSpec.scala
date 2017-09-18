package ch4

import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  "map" should "map value if it is present" in {
    (None: Option[Int]).map(_ + 1) should be(None)
    Some(1).map(_ + 1) should be(Some(2))
  }

  // flatMap
  // getOrElse
  // orElse
  // filter

}
