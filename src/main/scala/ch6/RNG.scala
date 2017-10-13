package ch6

trait RNG {
  def nextInt: (RNG, Int)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (RNG, Int) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt

      (nextRNG, n)
    }
  }

  def nonNegativeInt(rng: RNG): (RNG, Int) = {
    val (nextRng, n) = rng.nextInt
    (nextRng, Math.abs(n))
  }

  def double(rng: RNG): (RNG, Double) = {
    val (rng1, i1) = nonNegativeInt(rng)
    val (rng2, i2) = nonNegativeInt(rng1)

    val (d1,d2) = if (i1<i2) (i1.toDouble, i2.toDouble) else (i2.toDouble, i1.toDouble)
    val res = d1/d2

    (rng2, res)
  }

  def intDouble(rng: RNG): (RNG, (Int, Double)) = {
    val (r2, i) = rng.nextInt
    val (r3, d) = double(r2)

    (r3, (i,d))
  }

  def doubleInt(rng: RNG): (RNG, (Double, Int)) = {
    val (r2, d) = double(rng)
    val (r3, i) = r2.nextInt

    (r3, (d,i))
  }

  def double3(rng: RNG): (RNG, (Double, Double, Double)) = {
    val (r1, d1) = double(rng)
    val (r2, d2) = double(r1)
    val (r3, d3) = double(r2)

    (r3, (d1,d2,d3))
  }
}