package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex3_8Spec extends FlatSpec with Matchers {

  "fold" should "map unto itself" in {
    val arg: List[Int] = List(1,2,3)
    val result: List[Int] = List.foldRight(arg, Nil:List[Int])(Cons(_, _))

    result should be(arg)
  }
}
