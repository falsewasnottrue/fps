package ch3

import org.scalatest.{FlatSpec, Matchers}

class Ex24Spec extends FlatSpec with Matchers {

  "startsWith" should "work" in {
    List.startsWith(List(1,2,3), Nil) should be(true)
    List.startsWith(List(1,2,3), List(1)) should be(true)
    List.startsWith(List(1,2,3), List(1,2,3)) should be(true)

    List.startsWith(List(1,2,3), List(2)) should be(false)
    List.startsWith(Nil, List(2)) should be(false)

    List.startsWith(Nil, Nil) should be(true)
  }

  "hasSubsequence" should "work" in {
    List.hasSubsequence(List(1,2,3), Nil) should be(true)
    List.hasSubsequence(List(1,2,3), List(1,2,3)) should be(true)
    List.hasSubsequence(List(1,2,3), List(1,2)) should be(true)
    List.hasSubsequence(List(1,2,3), List(2,3)) should be(true)
    List.hasSubsequence(List(1,2,3), List(2)) should be(true)

    List.hasSubsequence(List(1,2,3), List(3,2)) should be(false)
    List.hasSubsequence(List(1,2,3), List(4)) should be(false)
    List.hasSubsequence(Nil, List(4)) should be(false)

    List.hasSubsequence(Nil, Nil) should be(false)
  }
}
