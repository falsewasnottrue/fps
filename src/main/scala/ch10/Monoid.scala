package ch10

import ch8.Gen

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

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 + a2
    override def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A) = a2 andThen a1
    override def zero = identity[A]
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override def zero: A = m.zero
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero) {
      case (b, a) => m.op(b, f(a))
    }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)
}

object MonoidLaws {
  import ch8.Prop._
  import ch8.Gen._

  def associativity[A](m: Monoid[A])(ga: Gen[A]): Prop = {
    forAll(listOfN(3, ga)) { l =>
      val a :: b :: c :: Nil = l
      m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)
    }
  }

  def neutralElement[A](m: Monoid[A])(ga: Gen[A]): Prop = {
    forAll(ga) { a =>
      m.op(m.zero, a) == a && m.op(a, m.zero) == a
    }
  }

  def laws[A](m: Monoid[A])(ga: Gen[A]): Prop =
    associativity(m)(ga) && neutralElement(m)(ga)
}