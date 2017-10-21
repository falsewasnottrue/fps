package ch11

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  override def map[A, B](fa: M[A])(f: A => B): M[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List[B]()))((a, mla) => map2(f(a), mla)(_ :: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    if (n==0) unit(List[A]()) else map2(ma, replicateM(n-1, ma))(_ :: _)
}

object OptionMonad extends Monad[Option] {
  override def unit[A](a: => A) = Some(a)
  override def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
}