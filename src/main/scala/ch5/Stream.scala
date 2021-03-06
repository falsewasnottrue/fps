package ch5

sealed trait Stream[+A] {
  import Stream._

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
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
    case Cons(h, _) if !pred(h()) => false
    case Cons(_, t) => t().forAll(pred)
    case Empty => true
  }

  def foldRight[B](z: B)(f: (A,B) => B): B = this match {
    case Empty => z
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
  }

  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(pred: A => Boolean): Stream[A] = foldRight(empty[A]) {
    case (h,t) => if (pred(h)) cons(h, t.filter(pred)) else t.filter(pred)
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B]) {
    case (h,t) => f(h).append(t)
  }

  def zip[B](other: Stream[B]): Stream[(A,B)] = (this, other) match {
    case (Cons(h,t), Cons(oh, ot)) => Cons(() => (h(), oh()), () => t().zip(ot()))
    case _ => empty
  }

  def find(p: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) if p(h()) => Some(h())
    case Cons(_, t) => t().find(p)
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

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def unfold[A, B](start: B)(f: B => Option[Tuple2[B,A]]): Stream[A] = f(start) match {
    case Some((next, elem)) => cons(elem, unfold(next)(f))
    case None => empty
  }
}
