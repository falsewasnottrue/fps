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

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r2) = rng.nextInt
    val (d, r3) = double(r2)

    ((i,d), r3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r2) = double(rng)
    val (i, r3) = r2.nextInt

    ((d,i), r3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1,d2,d3), r3)
  }
}