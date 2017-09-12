import org.scalatest.{FlatSpec, Matchers}

class Ex2_4Spec extends FlatSpec with Matchers {

  /**
    * Implement uncurry, which reverses the transformation of curry. Note that since =>
    * associates to the right, A => (B => C) can be written as A => B => C.
    */

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a,b) => f(a)(b)

  it should "uncurry addition" in {
    val addN: Int => Int => Int = a => b => a+b
    val add: (Int, Int) => Int = uncurry(addN)

    add(3,4) should be(7)
  }
}
