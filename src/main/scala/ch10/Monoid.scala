package ch10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String) = a1 + a2
    override def zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]) = a1 ++ a2
    override def zero = Nil
  }

  // intAddition
  // intmultiplication
  // booleanOr
  // booleanAnd
}