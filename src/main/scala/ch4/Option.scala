package ch4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case _ => None
  }
  def getOrElse[B >: A](alt: => B): B = this match {
    case Some(a) => a
    case _ => alt
  }
  def orElse[B >: A](alt: Option[B]): Option[B] = this match {
    case Some(_) => this
    case _ => alt
  }
  def filter(pred: A => Boolean): Option[A] = this match {
    case Some(a) if pred(a) => this
    case _ => None
  }
}

case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

object Option {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match {
    case (Some(aa), Some(bb)) => Some(f(aa, bb))
    case _ => None
  }

  // ex 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def sequenceAcc(acc: Option[List[A]], ls: List[Option[A]]): Option[List[A]] = (acc, ls) match {
      case (_, Nil) => acc
      case (_, None :: _) => None
      case (Some(as), Some(aa) :: tail) => sequenceAcc(Some(as :+ aa), tail)
    }

    sequenceAcc(Some(Nil), a)
  }

  // ex 4.5
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def traverseAcc(acc: Option[List[B]], ls: List[A]): Option[List[B]] = (acc, ls) match {
      case (_, Nil) => acc
      case (Some(as), aa :: tail) => f(aa) match {
        case Some(v) => traverseAcc(Some(as :+ v), tail)
        case _ => None
      }
    }

    traverseAcc(Some(Nil), a)
  }

  def traverseNaive[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a map f)
}