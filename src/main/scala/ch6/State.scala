package ch6

case class State[S, A](x: S => (S, A)) {
  
  def unit[S, A](a: A): State[S, A] = ???

  def map[B](f: A => B): State[S, B] = ???

  def flatMap[B](f: A => State[S, B]): State[S, B] = ???

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = ???

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = ???
}
