package ch5

sealed trait Stream[+A] {
  import Stream._

  def toList(): List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n-1))
    case _ => Empty
  }

  def takeWhile(pred: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if pred(h()) => cons(h(), t().takeWhile(pred))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_,t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def dropWhile(pred: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if pred(h()) => t().dropWhile(pred)
    case _ => this
  }

  def exists(pred: A => Boolean): Boolean = this match {
    case Cons(h, _) if pred(h()) => true
    case Cons(_, t) => t().exists(pred)
    case Empty => false
  }

  def forAll(pred: A => Boolean): Boolean = this match {
    case Cons(h, t) if !pred(h()) => false
    case Cons(_, t) => t().forAll(pred)
    case Empty => true
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
