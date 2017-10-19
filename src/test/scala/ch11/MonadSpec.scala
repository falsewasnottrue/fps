package ch11

import org.scalatest.{FlatSpec, Matchers}

class MonadSpec extends FlatSpec with Matchers {

  "OptionMonad" should "implement monad API" in {
    OptionMonad.unit(1) should be(Some(1))
    OptionMonad.flatMap(Some(1))(i => Some(i+1)) should be(Some(2))
  }
}
