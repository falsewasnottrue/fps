package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_10Spec extends FlatSpec with Matchers {

  "foldLeft" should "fold a list from the left" in {
    List.foldLeft(Nil: List[Int], 0)(_ + _) should be(0)
    List.foldLeft(List(1,2,3), 0)(_ + _) should be(6)
  }
}
