package ch2

import org.scalatest.{FlatSpec, Matchers}

class Ex1Spec extends FlatSpec with Matchers {

  /**
    *  * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s). The first two Fibonacci numbers are 0 and 1.
    *  The nth number is always the sum of the previous two—the sequence begins 0, 1, 1, 2, 3, 5.
    *  Your definition should use a local tail-recursive function.
    */

  def fib(n: Int): Int = fibTailRecursive(n)

  def fibNaiveRecursive(n: Int): Int =
    if (n==0) 0
    else if (n==1) 1
    else fib(n-1) + fib(n-2)

  def fibTailRecursive(n: Int): Int =  {
    def fib(n: Int, a: Int, b: Int): Int =
      if (n==0) a
      else if (n==1) b
      else fib(n-1, b, a+b)

    fib(n, 0, 1)
  }

  it should "return 0 at position 0" in {
    fib(0) should be(0)
  }

  it should "return 1 at position 1" in {
    fib(1) should be(1)
  }

  it should "return 1 at position 2" in {
    fib(2) should be(1)
  }

  it should "return 2 at position 3" in {
    fib(3) should be(2)
  }

  it should "return 3 at position 4" in {
    fib(4) should be(3)
  }

  it should "return 5 at position 5" in {
    fib(5) should be(5)
  }

  it should "return 55 at position 10" in {
    fib(10) should be(55)
  }
}
