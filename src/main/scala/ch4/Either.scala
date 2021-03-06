package ch4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E,B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
  def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case _ => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] = (this, b) match {
    case (Right(aa), Right(bb)) => Right(f(aa, bb))
    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    def sequenceAcc(acc: Either[E, List[A]], ls: List[Either[E, A]]): Either[E, List[A]] = (acc, ls) match {
      case (_, Nil) => acc
      case (Right(as), Right(l) :: ltail) => sequenceAcc(Right(as :+ l), ltail)
      case (_, Left(e) :: _) => Left(e)
    }

    sequenceAcc(Right(Nil), es)
  }

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sequence(es map f)
}