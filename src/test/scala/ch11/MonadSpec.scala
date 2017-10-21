package ch11

import org.scalatest.{FlatSpec, Matchers}

class MonadSpec extends FlatSpec with Matchers {

  "OptionMonad" should "implement monad API" in {
    OptionMonad.unit(1) should be(Some(1))
    OptionMonad.flatMap(Some(1))(i => Some(i+1)) should be(Some(2))
  }

  "sequence" should "be implemented for all monads" in {
    OptionMonad.sequence(List(Some(1), Some(2), Some(3))) should be(Some(List(1,2,3)))
    OptionMonad.sequence(List(Some(1), None, Some(3))) should be(None)
  }

  "traverse" should "be implemented for all monads" in {
    OptionMonad.traverse(List(1,2,3))(Some(_)) should be(Some(List(1,2,3)))
    OptionMonad.traverse(List(1,2,3))(i => if (i%2 == 0) Some(i) else None) should be(None)
  }

  "replicateM" should "be implemented for all monads" in {
    OptionMonad.replicateM(3, Some(3)) should be(Some(List(3,3,3)))
    OptionMonad.replicateM(3, None) should be(None)
  }
}
