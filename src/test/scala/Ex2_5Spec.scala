import org.scalatest.{FlatSpec, Matchers}

class Ex2_5Spec extends FlatSpec with Matchers {

  /**
    * Implement the higher-order function that composes two functions.
    */

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  it should "compose functions" in {
    val double = (a:Int) => 2 * a
    val toString = (a:Int) => "new value " + a
    val c = compose(toString, double)

    c(1) should be("new value 2")
  }
}
