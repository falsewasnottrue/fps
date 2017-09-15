package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_14Spec extends FlatSpec with Matchers {

  "appendViaFoldRight" should "append two lists" in {
    List.appendViaFoldRight(Nil, Nil) should be(Nil)
    List.appendViaFoldRight(Nil, List(3,4)) should be(List(3,4))
    List.appendViaFoldRight(List(1,2), Nil) should be(List(1,2))
    List.appendViaFoldRight(List(1,2), List(3,4)) should be(List(1,2,3,4))
  }

}
