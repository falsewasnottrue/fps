package ch3

sealed trait List[+A] {
  def tail: List[A]
  def setHead[B >: A](h: B): List[B]
}

case object Nil extends List[Nothing] {
  val tail = Nil
  def setHead[B >: Nothing](h: B): List[B] = Cons(h, Nil)
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  def setHead[B >: A](h: B): List[B] = Cons(h, tail)
}

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
