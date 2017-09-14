package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_5Spec extends FlatSpec with Matchers {

  "dropWhile" should "remove elements while predicate is true" in {
    List.dropWhile(List(1,2,3,4))(_ < 3) should be(List(3,4))
    List.dropWhile(List(1,2,3,4))(_ < 0) should be(List(1,2,3,4))
  }

  it should "do nothing on an empty list" in  {
    List.dropWhile(Nil:List[Int])(_ < 3) should be(Nil)
  }
}
