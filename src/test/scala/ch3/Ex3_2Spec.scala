package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_2Spec extends FlatSpec with Matchers {

  "tail" should "remove the first element of a list" in {
    List(1,2,3).tail should be(List(2,3))
  }

  it should "return Nil on an empty list" in {
    Nil.tail should be(Nil)
  }

}
