package ch8

import ch5.Stream
import ch6.RNG
import ch8.Prop.{FailedCase, SuccessCount, TestCases}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
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

case class Prop(run: (TestCases, RNG) => Result) {

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop(
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map{ case (a,i) =>
      try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  )
}

