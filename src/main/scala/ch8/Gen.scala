package ch8

import ch6.{RNG, State}

object Gen {

  type Gen[A] = State[RNG, A]

  def choose(start: Int, end: Int): Gen[Int] =
    State(rng => {
      val (value, nextRNG) = rng.nextInt

      (nextRNG, value % (end-start) + start)
    })

}
