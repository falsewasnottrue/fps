package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_3Spec extends FlatSpec with Matchers {

  "setHead" should "replace the first element of a list" in {
    List(1,2,3).setHead(4) should be(List(4,2,3))
  }

  it should "also work on empty lists ... maybe" in {
    Nil.setHead(1) should be(List(1))
  }
}
