package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_12Spec extends FlatSpec with Matchers {

  "reverse" should "reverse a list" in {
    List.reverse(Nil) should be(Nil)
    List.reverse(List(1,2,3)) should be(List(3,2,1))
  }
}
