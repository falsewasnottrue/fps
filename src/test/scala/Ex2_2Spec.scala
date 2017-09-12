import org.scalatest.{FlatSpec, Matchers}

class Ex2_2Spec extends FlatSpec with Matchers {

  /**
   * Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function:
   */

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = as match {
    case Array() => true
    case Array(_) => true
    case Array(a, b, _*) => ordered(a,b) && isSorted(as.tail, ordered)
    case _ => false
  }

  it should "return true for an empty array" in {
    isSorted[Int](Array[Int](), _ < _) should be(true)
  }

  it should "return true for an array with a single element" in {
    isSorted[Int](Array[Int](1), _ < _) should be(true)
  }

  it should "return true for sorted array with more than one element" in {
    isSorted[Int](Array[Int](1, 2), _ < _) should be(true)
  }

  it should "return true for sorted array with more than one element 2" in {
    isSorted[Int](Array[Int](1, 2, 3, 4, 5, 6), _ < _) should be(true)
  }

  it should "return false for unsorted array with more than one element" in {
    isSorted[Int](Array[Int](2, 1), _ < _) should be(false)
  }

  it should "return false for unsorted array with more than one element 2" in {
    isSorted[Int](Array[Int](1, 2, 3, 4, 1, 5, 6), _ < _) should be(false)
  }
}
