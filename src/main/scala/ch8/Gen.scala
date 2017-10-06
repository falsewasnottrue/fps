package ch8

import ch6.{RNG, State}

case class Gen[A](sample: State[RNG,A], exhaustive: Stream[A])

object Gen {

  def choose(start: Int, end: Int): Gen[Int] =
    Gen(
      State(rng => {
        val (value, nextRNG) = rng.nextInt

        (nextRNG, value % (end-start) + start)
      }),
      Stream.empty[Int]
    )
}
