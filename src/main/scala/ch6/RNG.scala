package ch6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) =rng.nextInt

    (Math.abs(n), nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i1, rng1) = nonNegativeInt(rng)
    val (i2, rng2) = nonNegativeInt(rng1)

    val (d1,d2) = if (i1<i2) (i1.toDouble, i2.toDouble) else (i2.toDouble, i1.toDouble)
    val res = d1/d2

    (res, rng2)
  }

}