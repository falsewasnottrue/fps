package ch8

import ch5.Stream
import ch6.RNG
import ch8.Prop._

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

    def &&(p: Prop): Prop = Prop((maxSize, n, rng) => {
      val thisRun = this.run(maxSize, n, rng)
      val thatRun = p.run(maxSize,n , rng)

      (thisRun, thatRun) match {
        case (Passed, Passed) => Passed
        case (Falsified(f,s), _) => Falsified(f,s)
        case (_, f) => f
      }
    })

    def ||(p: Prop): Prop = Prop((maxSize, n, rng) => {
      val thisRun = this.run(maxSize, n, rng)
      val thatRun = p.run(maxSize,n , rng)

      (thisRun, thatRun) match {
        case (Passed, _) => Passed
        case (_, Passed) => Passed
        case (f, _) => f
      }
    })
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop(
    (max, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map{ case (a,i) =>
      try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  )

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop((max, n, rng) => {
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] =
      Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
    val prop: Prop =
      props.map(p => Prop{ (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
    prop.run(max, n, rng)
  })

  def run(p: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failured: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}
