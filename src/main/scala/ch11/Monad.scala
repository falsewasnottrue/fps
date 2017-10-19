package ch11

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  override def map[A, B](fa: M[A])(f: A => B) =
    flatMap(fa)(a => unit(f(a)))

  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))
}

object OptionMonad extends Monad[Option] {
  override def unit[A](a: => A) = Some(a)
  override def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
}