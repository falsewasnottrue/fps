package ch8

import ch6.{RNG, State}

case class Gen[A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {

  def unit[A](a: => A): Gen[A] = Gen(
    State(rng => (rng, a))
  )

  def ints: Gen[Int] = Gen(State(rng => rng.nextInt))

  def choose(start: Int, end: Int): Gen[Int] =
    ints.map(i => i % (end - start) + start)

  def boolean: Gen[Boolean] = ints.map(_ % 2 == 0)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    if (n==0) unit(Nil) else listOfN(n-1, g).flatMap(ls => g.map(a => a :: ls))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(i => Gen.listOfN(i, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(i => Gen.listOfN(i+1, g))
}
