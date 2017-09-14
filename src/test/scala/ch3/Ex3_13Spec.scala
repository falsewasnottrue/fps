package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_13Spec extends FlatSpec with Matchers {

  "foldLeftViaFoldRight" should "work" in {
    List.foldLeftViaFoldRight(List[Int](), 0)(_ + _) should be(0)
    List.foldLeftViaFoldRight(List(1,2,3), 0)(_ + _) should be(6)
  }

  "foldRightViaFoldLeft" should "work" in {
    List.foldRightViaFoldLeft(List[Int](), 0)(_ + _) should be(0)
    List.foldRightViaFoldLeft(List(1,2,3), 0)(_ + _) should be(6)
  }
}
