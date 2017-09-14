package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_9Spec extends FlatSpec with Matchers {

  "length" should "return the length of a list" in {
    List.length(Nil) should be(0)
    List.length(List(1,4,9,16)) should be(4)
  }
}
