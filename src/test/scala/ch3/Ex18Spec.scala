package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex18Spec extends FlatSpec with Matchers {

  "map" should "work on empty list" in {
    List.map(List[Int]())(_ + 1) should be(Nil)
  }

  it should "map elements to same type" in {
    List.map(List(1,2,3))(_ + 1) should be(List(2,3,4))
  }

  it should "map elements to different type" in {
    List.map(List(1,2,3))(_ % 2 != 0) should be(List(true, false, true))
  }
}
