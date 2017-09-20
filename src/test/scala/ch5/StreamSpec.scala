package ch5

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  "toList" should "turn a stream to a list" in {
    Empty.toList() should be(Nil)
    Stream(1,2,3,4).toList() should be(List(1,2,3,4))
  }

  "take" should "return a prefix stream" in {
    Stream(1,2,3,4).take(2).toList() should be(List(1,2))
    Stream(1,2,3,4).take(10).toList() should be(List(1,2,3,4))

    Empty.take(10).toList() should be(Nil)
  }

  "takeWhile" should "return a prefix stream" in {
    Stream(1,2,3,4).takeWhile(_ < 3).toList() should be(List(1,2))
    Stream(1,2,3,4).takeWhile(_ < 10).toList() should be(List(1,2,3,4))

    (Empty: Stream[Int]).takeWhile(_ < 10).toList() should be(Nil)
  }

  "drop" should "remove a prefix stream" in {
    Stream(1,2,3,4).drop(2).toList() should be(List(3,4))
    Stream(1,2,3,4).drop(10).toList() should be(Nil)

    Empty.drop(10).toList() should be(Nil)
  }

  "dropWhile" should "remove a prefix stream" in {
    Stream(1,2,3,4).dropWhile(_ < 3).toList() should be(List(3,4))
    Stream(1,2,3,4).dropWhile(_ < 10).toList() should be(Nil)

    (Empty: Stream[Int]).dropWhile(_ < 5).toList() should be(Nil)
  }

  "exists" should "return true iff there is an element for which the predicate holds" in {
    Stream(1,2,3,4).exists(_ == 3) should be(true)
    Stream(1,2,3,4).exists(_ == 5) should be(false)
    (Empty: Stream[Int]).exists(_ == 3) should be(false)
  }

}
