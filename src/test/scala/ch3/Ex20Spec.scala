package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex20Spec extends FlatSpec with Matchers {

  "flatMap" should "work" in {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) should be(List(1,1,2,2,3,3))
    List.flatMap(Nil)(i => List(i,i)) should be(Nil)
    List.flatMap(List(1, 2, 3))(i => Nil) should be(Nil)
  }
}
