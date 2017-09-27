package ch6

object Rand {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A=>B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt: Rand[Int] = rng => {
    val (n, nextRNG) = rng.nextInt
    (Math.abs(n), nextRNG)
  }

  def nonNegativeEvent: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))


  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)

    (f(a,b), rng2)
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
    val (a, next) = ra(rng)
    f(a)(next)
  }

}