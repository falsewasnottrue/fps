package ch5

import org.scalatest.{FlatSpec, Matchers}
import Stream._

class StreamSpec extends FlatSpec with Matchers {

  "toList" should "turn a stream to a list" in {
    toList(Empty) should be(Nil)
    toList(Stream(1,2,3,4)) should be(List(1,2,3,4))
  }
}
