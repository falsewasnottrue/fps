package ch4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = None
  def flatMap[B](f: A => Option[B]): Option[B] = None
}

case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))
  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)
}

case object None extends Option[Nothing]
