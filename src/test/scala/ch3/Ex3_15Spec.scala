package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_15Spec extends FlatSpec with Matchers {

  "concat" should "reduce a list of lists" in {
    List.concat(List(Nil, Nil)) should be(Nil)
    List.concat(List(List(1,2), List(3,4))) should be(List(1,2,3,4))
    List.concat(List(List(1,2), List(3,4), List(5,6), List(7,8))) should be(List(1,2,3,4,5,6,7,8))
  }
}
