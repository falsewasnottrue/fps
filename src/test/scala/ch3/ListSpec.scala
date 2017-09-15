package ch3

import org.scalatest.{FlatSpec, Matchers}
import List._

class ListSpec extends FlatSpec with Matchers {

  "tail" should "remove the first element of a list" in {
    tail(List(1,2,3)) should be(List(2,3))
  }

  it should "return Nil on an empty list" in {
    tail(Nil) should be(Nil)
  }

  "setHead" should "replace the first element of a list" in {
    setHead(List(1,2,3), 4) should be(List(4,2,3))
  }

  it should "also work on empty lists ... maybe" in {
    setHead(Nil, 1) should be(List(1))
  }

  "drop" should "return list if n is 0" in {
    drop(Nil, 0) should be(Nil)
    drop(List(1,2,3), 0) should be(List(1,2,3))
  }

  it should "drop (at most) the specified number of elements" in {
    drop(List(1,2,3), 2) should be(List(3))
    drop(List(1,2,3), 3) should be(Nil)
    drop(List(1,2,3), 7) should be(Nil)

    drop(Nil, 1) should be(Nil)
  }

  "dropWhile" should "remove elements while predicate is true" in {
    dropWhile(List(1,2,3,4))(_ < 3) should be(List(3,4))
    dropWhile(List(1,2,3,4))(_ < 0) should be(List(1,2,3,4))
  }

  it should "do nothing on an empty list" in  {
    dropWhile(Nil:List[Int])(_ < 3) should be(Nil)
  }

  "init" should "return the initial part of the list (except the last element)" in {
    init(List(1,2,3,4)) should be(List(1,2,3))
    init(List(1)) should be(Nil)
    init(Nil) should be(Nil)
  }

  "sum2" should "calculate the sum of a list" in {
    sum2(Nil) should be(0)
    sum2(List(1,2,3)) should be(6)
  }

  "product2" should "calculate the product of a list" in {
    product2(Nil) should be(1.0)
    product2(List(1.0,2.0,3.0)) should be(6.0)
  }

  "foldRight" should "fold a list from the right" in {
    val arg: List[Int] = List(1,2,3)
    val result: List[Int] = foldRight(arg, Nil:List[Int])(Cons(_, _))

    result should be(arg)
  }

  "length" should "return the length of a list" in {
    List.length(Nil) should be(0)
    List.length(List(1,4,9,16)) should be(4)
  }

  "foldLeft" should "fold a list from the left" in {
    foldLeft(Nil: List[Int], 0)(_ + _) should be(0)
    foldLeft(List(1,2,3), 0)(_ + _) should be(6)
  }

  "sum3" should "calculate the sum" in {
    sum3(Nil) should be(0)
    sum3(List(1,2,3,4)) should be(10)
  }

  "product3" should "calculate the product" in {
    product3(Nil) should be(1.0)
    product3(List(1,2,3,4)) should be(24.0)
  }

  "length2" should "calculate the length" in {
    length2(Nil) should be(0)
    length2(List(2,3,4,5)) should be(4)
  }

  "reverse" should "reverse a list" in {
    reverse(Nil) should be(Nil)
    reverse(List(1,2,3)) should be(List(3,2,1))
  }

  "foldLeftViaFoldRight" should "work" in {
    foldLeftViaFoldRight(List[Int](), 0)(_ + _) should be(0)
    foldLeftViaFoldRight(List(1,2,3), 0)(_ + _) should be(6)
  }

  "foldRightViaFoldLeft" should "work" in {
    foldRightViaFoldLeft(List[Int](), 0)(_ + _) should be(0)
    foldRightViaFoldLeft(List(1,2,3), 0)(_ + _) should be(6)
  }

  "appendViaFoldRight" should "append two lists" in {
    appendViaFoldRight(Nil, Nil) should be(Nil)
    appendViaFoldRight(Nil, List(3,4)) should be(List(3,4))
    appendViaFoldRight(List(1,2), Nil) should be(List(1,2))
    appendViaFoldRight(List(1,2), List(3,4)) should be(List(1,2,3,4))
  }

  "concat" should "reduce a list of lists" in {
    concat(List(Nil, Nil)) should be(Nil)
    concat(List(List(1,2), List(3,4))) should be(List(1,2,3,4))
    concat(List(List(1,2), List(3,4), List(5,6), List(7,8))) should be(List(1,2,3,4,5,6,7,8))
  }

  "map" should "work on empty list" in {
    map(List[Int]())(_ + 1) should be(Nil)
  }

  it should "map elements to same type" in {
    map(List(1,2,3))(_ + 1) should be(List(2,3,4))
  }

  it should "map elements to different type" in {
    map(List(1,2,3))(_ % 2 != 0) should be(List(true, false, true))
  }

  "removeOdds" should "remove odd ints" in {
    removeOdds(Nil) should be(Nil)
    removeOdds(List(1,2,3,4,5,6)) should be(List(2,4,6))
    removeOdds(List(1,3,5)) should be(Nil)
  }

  "flatMap" should "work" in {
    flatMap(List(1, 2, 3))(i => List(i, i)) should be(List(1,1,2,2,3,3))
    flatMap(Nil)(i => List(i,i)) should be(Nil)
    flatMap(List(1, 2, 3))(i => Nil) should be(Nil)
  }

  "addPairwise" should "add ints pairwise" in {
    addPairwise(List(1,2,3), List(4,5,6)) should be(List(5,7,9))
    addPairwise(List(1,2,3), List(4)) should be(List(5))

    addPairwise(Nil, Nil) should be(Nil)
    addPairwise(List(1,2,3), Nil) should be(Nil)
    addPairwise(Nil, List(1,2,3)) should be(Nil)
  }

  "zipWith" should "zip two lists" in {
    zipWith(List(1,2,3), List(4,5,6))(_+_) should be(List(5,7,9))
    zipWith(List(1,2,3), List(4,5,6))(_*_) should be(List(4,10,18))

    zipWith(List(1,2,3), Nil)(_+_) should be(Nil)
    zipWith(List[Int](), List(1,2,3))(_+_) should be(Nil)

    zipWith(List[Int](), Nil)(_+_) should be(Nil)
  }

  "startsWith" should "work" in {
    startsWith(List(1,2,3), Nil) should be(true)
    startsWith(List(1,2,3), List(1)) should be(true)
    startsWith(List(1,2,3), List(1,2,3)) should be(true)

    startsWith(List(1,2,3), List(2)) should be(false)
    startsWith(Nil, List(2)) should be(false)

    startsWith(Nil, Nil) should be(true)
  }

  "hasSubsequence" should "work" in {
    hasSubsequence(List(1,2,3), Nil) should be(true)
    hasSubsequence(List(1,2,3), List(1,2,3)) should be(true)
    hasSubsequence(List(1,2,3), List(1,2)) should be(true)
    hasSubsequence(List(1,2,3), List(2,3)) should be(true)
    hasSubsequence(List(1,2,3), List(2)) should be(true)

    hasSubsequence(List(1,2,3), List(3,2)) should be(false)
    hasSubsequence(List(1,2,3), List(4)) should be(false)
    hasSubsequence(Nil, List(4)) should be(false)

    hasSubsequence(Nil, Nil) should be(false)
  }
}
