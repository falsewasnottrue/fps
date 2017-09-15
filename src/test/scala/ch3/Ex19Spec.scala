package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex19Spec extends FlatSpec with Matchers {

  "removeOdds" should "remove odd ints" in {
    List.removeOdds(Nil) should be(Nil)
    List.removeOdds(List(1,2,3,4,5,6)) should be(List(2,4,6))
    List.removeOdds(List(1,3,5)) should be(Nil)
  }
}
