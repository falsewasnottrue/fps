package ch8

case class SGen[A](forSize: Int => Gen[A]) {

  def map[B](f: A => B): SGen[B] = SGen(i => forSize(i).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(i => forSize(i).flatMap(a => f(a).forSize(i)))
}
