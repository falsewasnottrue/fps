package ch11

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

object ListFunctor extends Functor[List] {
  override def map[A, B](fa: List[A])(f: A => B) = fa map f
}
