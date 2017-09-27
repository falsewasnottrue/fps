package ch6

import org.scalatest.{FlatSpec, Matchers}
import State._

class StateSpec extends FlatSpec with Matchers {

  it should "work" in {
    import Candy._

    val inputCoin = List(Coin)
    val inputTurn = List(Turn)

    // Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
    val machine1 = Machine(locked = true, 1, 0)
    simulateMachine(inputCoin).run(machine1)._1.locked should be(false)

    // Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
    val machine2 = Machine(locked = false, 1, 1)
    val m2Result = simulateMachine(inputTurn).run(machine2)
    m2Result._1.locked should be(true)
    m2Result._1.candies should be(0)

    // Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
    simulateMachine(inputTurn).run(machine1)._1.locked should be(machine1.locked)
    simulateMachine(inputCoin).run(machine2)._1.locked should be(machine2.locked)

    // A machine that’s out of candy ignores all inputs.
    val machine3 = Machine(locked = true, 0, 1)
    simulateMachine(inputTurn).run(machine3)._1.locked should be(machine3.locked)
    simulateMachine(inputCoin).run(machine3)._1.locked should be(machine3.locked)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update =
    (i: Input) =>
      (s: Machine) =>
        (i, s) match {
          case (_, Machine(_, 0, _)) => s
          case (Coin, Machine(false, _, _)) => s
          case (Turn, Machine(true, _, _)) => s
          case (Coin, Machine(true, candy, coin)) =>
            Machine(false, candy, coin + 1)
          case (Turn, Machine(false, candy, coin)) =>
            Machine(true, candy - 1, coin)
        }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)
}
