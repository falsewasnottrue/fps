package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex23Spec extends FlatSpec with Matchers {

  "zipWith" should "zip two lists" in {
    List.zipWith(List(1,2,3), List(4,5,6))(_+_) should be(List(5,7,9))
    List.zipWith(List(1,2,3), List(4,5,6))(_*_) should be(List(4,10,18))

    List.zipWith(List(1,2,3), Nil)(_+_) should be(Nil)
    List.zipWith(List[Int](), List(1,2,3))(_+_) should be(Nil)

    List.zipWith(List[Int](), Nil)(_+_) should be(Nil)
  }

}
