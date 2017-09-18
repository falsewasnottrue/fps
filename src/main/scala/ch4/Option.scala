package ch4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = None
  def flatMap[B](f: A => Option[B]): Option[B] = None
  def getOrElse[B >: A](alt: B): B = alt
  def orElse[B >: A](alt: Option[B]): Option[B] = alt
  def filter(pred: A => Boolean): Option[A] = None
}

case object None extends Option[Nothing]

case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))
  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)
  override def getOrElse[B >: A](alt: B): B = get
  override def orElse[B >: A](alt: Option[B]): Option[B] = this
  override def filter(pred: A => Boolean): Option[A] = if (pred(get)) this else None
}

object Option {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match {
    case (Some(aa), Some(bb)) => Some(f(aa, bb))
    case _ => None
  }
}