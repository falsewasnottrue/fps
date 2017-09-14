package ch3

import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {

  "sum2" should "calculate the sum of a list" in {
    List.sum2(Nil) should be(0)
    List.sum2(List(1,2,3)) should be(6)
  }

  "product2" should "calculate the product of a list" in {
    List.product2(Nil) should be(1.0)
    List.product2(List(1.0,2.0,3.0)) should be(6.0)
  }
}
