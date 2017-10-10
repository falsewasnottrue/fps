package ch6

object Rand {
  type Rand[+A] = RNG => (RNG, A)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (rng, a)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (rng2, a) = s(rng)
      (rng2, f(a))
    }

  def nonNegativeInt: Rand[Int] = rng => {
    val (nextRNG, n) = rng.nextInt
    (nextRNG, Math.abs(n))
  }

  def nonNegativeEvent: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))


  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
    val (rng1, a) = ra(rng)
    val (rng2, b) = rb(rng1)

    (rng2, f(a,b))
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_,_))

  def intDouble: Rand[(Int, Double)] = both(int, double)

  def doubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](ls: List[Rand[A]]): Rand[List[A]] = ls match {
    case Nil => unit(Nil)
    case ra :: ras => map2(ra, sequence(ras))((a, as) => a :: as)
  }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](ra: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (next, a) = ra(rng)
    f(a)(next)
  }

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))
}