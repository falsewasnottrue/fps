package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_4Spec extends FlatSpec with Matchers {

  "drop" should "return list if n is 0" in {
    List.drop(Nil, 0) should be(Nil)
    List.drop(List(1,2,3), 0) should be(List(1,2,3))
  }

  it should "drop (at most) the specified number of elements" in {
    List.drop(List(1,2,3), 2) should be(List(3))
    List.drop(List(1,2,3), 3) should be(Nil)
    List.drop(List(1,2,3), 7) should be(Nil)

    List.drop(Nil, 1) should be(Nil)
  }
}