package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex22Spec extends FlatSpec with Matchers {

  "addPairwise" should "add ints pairwise" in {
      List.addPairwise(List(1,2,3), List(4,5,6)) should be(List(5,7,9))
      List.addPairwise(List(1,2,3), List(4)) should be(List(5))

      List.addPairwise(Nil, Nil) should be(Nil)
      List.addPairwise(List(1,2,3), Nil) should be(Nil)
      List.addPairwise(Nil, List(1,2,3)) should be(Nil)
  }
}
