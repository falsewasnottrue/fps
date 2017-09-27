package ch6

case class State[S, A](run: S => (S, A)) {

  import State._

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (next, a) = run(s)
    f(a).run(next)
  })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (s, a))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S,List[A]](Nil))((sa, acc) => sa.map2(acc)(_ :: _))
}