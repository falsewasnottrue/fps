package ch7

import java.util.concurrent.{ExecutorService, Executors, Future, ThreadPoolExecutor}

import org.scalatest.{FlatSpec, Matchers}
import Par._

class ParSpec extends FlatSpec with Matchers {

  val es: ExecutorService = Executors.newCachedThreadPool()

  "sequence" should "combine pars" in {
    val s: Par[List[Int]] = sequence(List(unit(1), unit(2)))

    val res: Future[List[Int]] = s(es)
    res.get should be(List(1,2))
  }

  "parFilter" should "filter a list in parallel" in {
    val res: Future[List[Int]] = parFilter(List(1,2,3,4))(_ % 2 == 0)(es)
    res.get should be(List(2,4))
  }

  "choiceCond" should "work implemented by flatMap" in {
    choiceCond(unit(true))(unit(1), unit(2))(es).get should be(1)
    choiceCond(unit(false))(unit(1), unit(2))(es).get should be(2)
  }

  "choiceN" should "work implemented by flatMap" in {
    choiceN(unit(0))(List(unit(1), unit(2), unit(3)))(es).get should be(1)
    choiceN(unit(1))(List(unit(1), unit(2), unit(3)))(es).get should be(2)
    choiceN(unit(2))(List(unit(1), unit(2), unit(3)))(es).get should be(3)
  }

  "choiceMap" should "work implemented by flatMap" in {
    choiceMap(unit("a"))(Map("a" -> unit(1), "b" -> unit(7)))(es).get should be(1)
    choiceMap(unit("b"))(Map("a" -> unit(1), "b" -> unit(7)))(es).get should be(7)
  }

  "join" should "join a nested par" in {
    join(unit(unit(1)))(es).get should be(1)
  }
}
