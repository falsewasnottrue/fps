package ch11

import ch6.State

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  override def map[A, B](fa: M[A])(f: A => B): M[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List[B]()))((a, mla) => map2(f(a), mla)(_ :: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    if (n == 0) unit(List[A]()) else map2(ma, replicateM(n - 1, ma))(_ :: _)

  def filterM[A](as: List[A])(f: A => M[Boolean]): M[List[A]] =
    as.foldRight(unit(List[A]()))((a, mla) => map2(f(a), mla)((cond, la) => if (cond) a :: la else la))

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(b => g(b))
}

object OptionMonad extends Monad[Option] {
  override def unit[A](a: => A) = Some(a)
  override def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
}

case class Id[A](value: A) extends Monad[Id] {
  override def unit[A](a: => A) = Id(a)
  override def flatMap[A, B](ma: Id[A])(f: A => Id[B]) = f(value)
}

class StateMonads[S] {
  type StateS[A] = State[S, A]

  val monad = new Monad[StateS] {
    def unit[A](a: => A): State[S, A] = State(s => (s, a))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }
}