package ch10

import ch3.{Branch, Leaf, Tree}

trait Foldable[F[_]] {

  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
}

object TreeFoldable extends Foldable[Tree] {

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l,r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(mb.zero){ case (b,a) => mb.op(b,f(a)) }){ case (b,a) => mb.op(b,f(a)) }
  }
}