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
}
