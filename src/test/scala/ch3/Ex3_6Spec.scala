package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_6Spec extends FlatSpec with Matchers {

  "init" should "return the initial part of the list (except the last element)" in {
    List.init(List(1,2,3,4)) should be(List(1,2,3))
    List.init(List(1)) should be(Nil)
    List.init(Nil) should be(Nil)
  }
}
