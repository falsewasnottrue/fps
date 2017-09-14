package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_3Spec extends FlatSpec with Matchers {

  "setHead" should "replace the first element of a list" in {
    List.setHead(List(1,2,3), 4) should be(List(4,2,3))
  }

  it should "also work on empty lists ... maybe" in {
    List.setHead(Nil, 1) should be(List(1))
  }
}
