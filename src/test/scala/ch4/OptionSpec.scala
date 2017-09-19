package ch4

import org.scalatest.{FlatSpec, Matchers}
import Option._

class OptionSpec extends FlatSpec with Matchers {

  "map" should "map value if it is present" in {
    (None: Option[Int]).map(_ + 1) should be(None)
    Some(1).map(_ + 1) should be(Some(2))
  }

  "flatMap" should "flatMap value if it is present" in {
    Some(1).flatMap(i => Some(i * 3)) should be(Some(3))
    Some(1).flatMap(i => None) should be(None)
  }

  it should "return None if there is no value" in {
    (None: Option[Int]).flatMap(_ => Some(1)) should be(None)
    (None: Option[Int]).flatMap(_ => None) should be(None)
  }

  "getOrElse" should "return the value it is present" in {
    Some(1).getOrElse(2) should be(1)
  }

  it should "return the default if there is no value" in {
    None.getOrElse(2) should be(2)
  }

  "orElse" should "return this value if it is present" in {
    Some(1).orElse(Some(2)) should be(Some(1))
    Some(1).orElse(None) should be(Some(1))
  }

  it should "return the default it there is no value" in {
    None.orElse(Some(2)) should be(Some(2))
    None.orElse(None) should be(None)
  }

  "filter" should "return the value if the predicate holds" in {
    Some(1).filter(_ % 2 == 1) should be(Some(1))
  }

  it should "return none if the predicate is not true" in {
    Some(1).filter(_ % 2 == 0) should be(None)
  }

  it should "return none if there is no value" in {
    None.filter(_ => true) should be(None)
  }

  "map2" should "combine two options" in {
      map2(Some(1), Some(2))(_ + _) should be(Some(3))
  }

  it should "be None if either value is none" in {
    map2(None: Option[Int], Some(2))(_ * _) should be(None)
    map2(Some(2), None: Option[Int])(_ * _) should be(None)
    map2(None: Option[Int], None: Option[Int])(_ * _) should be(None)
  }

  "sequence" should "collect options" in {
    sequence(List(Some(1), Some(2), Some(3))) should be(Some(List(1,2,3)))
  }

  it should "return None if there is a single None in the collection" in {
    sequence(List(Some(1), None, Some(3))) should be(None)
  }

  // current implementation handles edge case like this:
  it should "return Some for the empty list" in {
    sequence(Nil) should be(Some(Nil))
  }

  "traverse" should "return Some if all elements are mapped to Some" in {
    traverse(List(1,2))(i => Some(i + 1)) should be(Some(List(2,3)))
  }

  it should "return None if only a single element is mapped to None" in {
    traverse(List(1,2))(i => if (i%2 == 1) Some(i) else None) should be(None)
  }
}

