package ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](ls: List[A], h: A): List[A] = ls match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def drop[A](ls: List[A], n: Int): List[A] =
    if (n==0) ls else drop(List.tail(ls), n-1)

  def dropWhile[A](ls: List[A])(p: A => Boolean): List[A] = ls match {
    case Nil => Nil
    case Cons(h, t) if p(h) => dropWhile(t)(p)
    case t => t
  }

  def init[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](ls: List[A], z: B)(f: (A,B) => B): B = ls match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t,z)(f))
  }

  def sum2(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)
  def product2(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)
}
