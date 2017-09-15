package ch2

import org.scalatest.{FlatSpec, Matchers}

class Ex3Spec extends FlatSpec with Matchers {

  /**
    * Let’s look at another example, currying, which converts a function f of two arguments
    * into a function of one argument that partially applies f. Here again there’s only one
    * implementation that compiles. Write this implementation.
    */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => f(a, _)

  it should "work" in {
    def addN = curry[Int, Int, Int](_ + _)
    def add1 = addN(1)

    add1(7) should be(8)
  }
}
