package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_11Spec extends FlatSpec with Matchers {

  "sum3" should "calculate the sum" in {
    List.sum3(Nil) should be(0)
    List.sum3(List(1,2,3,4)) should be(10)
  }

  "product3" should "calculate the product" in {
    List.product3(Nil) should be(1.0)
    List.product3(List(1,2,3,4)) should be(24.0)
  }

  "length2" should "calculate the length" in {
    List.length2(Nil) should be(0)
    List.length2(List(2,3,4,5)) should be(4)
  }
}
