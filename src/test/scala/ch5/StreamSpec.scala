package ch5

import org.scalatest.{FlatSpec, Matchers}
import Stream._

class StreamSpec extends FlatSpec with Matchers {

  "toList" should "turn a stream to a list" in {
    Empty.toList should be(Nil)
    Stream(1,2,3,4).toList should be(List(1,2,3,4))
  }

  "take" should "return a prefix stream" in {
    Stream(1,2,3,4).take(2).toList should be(List(1,2))
    Stream(1,2,3,4).take(10).toList should be(List(1,2,3,4))

    Empty.take(10).toList should be(Nil)
  }

  "takeWhile" should "return a prefix stream" in {
    Stream(1,2,3,4).takeWhile(_ < 3).toList should be(List(1,2))
    Stream(1,2,3,4).takeWhile(_ < 10).toList should be(List(1,2,3,4))

    (Empty: Stream[Int]).takeWhile(_ < 10).toList should be(Nil)
  }

  "drop" should "remove a prefix stream" in {
    Stream(1,2,3,4).drop(2).toList should be(List(3,4))
    Stream(1,2,3,4).drop(10).toList should be(Nil)

    Empty.drop(10).toList should be(Nil)
  }

  "dropWhile" should "remove a prefix stream" in {
    Stream(1,2,3,4).dropWhile(_ < 3).toList should be(List(3,4))
    Stream(1,2,3,4).dropWhile(_ < 10).toList should be(Nil)

    (Empty: Stream[Int]).dropWhile(_ < 5).toList should be(Nil)
  }

  "exists" should "return true iff there is an element for which the predicate holds" in {
    Stream(1,2,3,4).exists(_ == 3) should be(true)
    Stream(1,2,3,4).exists(_ == 5) should be(false)
    (Empty: Stream[Int]).exists(_ == 3) should be(false)
  }

  "forAll" should "return true iff predicate holds for all elements" in {
    Stream(1,2,3,4).forAll(_ < 10) should be(true)
    Stream(1,2,3,4).forAll(_ < 4) should be(false)
    (Empty: Stream[Int]).forAll(_ < 1) should be(true)
  }

  "foldRight" should "fold a stream from the right" in {
    Stream(1,2,3,4).foldRight(0)(_ + _) should be(10)
    Stream(1,2,3,4).foldRight(1)(_ * _) should be(24)
  }

  "headOption" should "return the stream head if there is one" in {
    Empty.headOption should be(None)
    Stream(1,2,3,4).headOption should be(Some(1))
  }

  "map" should "map values of a stream" in {
    Stream(1,2,3,4).map(_ + 1).toList should be(List(2,3,4,5))
    Stream(1,2,3,4).map(i => "nr " + i).toList should be(List("nr 1", "nr 2", "nr 3", "nr 4"))

    (Empty: Stream[Int]).map(_ + 1).toList should be(Nil)
  }

  "filter" should "filter a stream" in {
    Stream(1,2,3,4).filter(_ % 2 == 0).toList should be(List(2,4))
    Stream(1,2,3,4).filter(_ => true).toList should be(List(1,2,3,4))
    Stream(1,2,3,4).filter(_ => false).toList should be(Nil)

    Empty.filter(_ => true).toList should be(Nil)
  }

  "append" should "append two streams" in {
    Stream(1,2).append(Stream(3,4)).toList should be(List(1,2,3,4))
    Empty.append(Stream(3,4)).toList should be(List(3,4))
    Stream(1,2).append(Empty).toList should be(List(1,2))
  }

  "flatMap" should "flat map to a stream" in {
    Stream(1,2).flatMap(i => Stream(i*2, i*3)).toList should be(List(2,3,4,6))
    Stream(1,2).flatMap(i => Empty).toList should be(Nil)
    Empty.flatMap(i => Empty).toList should be(Nil)
  }
}
