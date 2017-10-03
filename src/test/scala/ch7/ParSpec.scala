package ch7

import java.util.concurrent.{ExecutorService, Executors, Future, ThreadPoolExecutor}

import org.scalatest.{FlatSpec, Matchers}
import Par._

class ParSpec extends FlatSpec with Matchers {

  "sequence" should "combine pars" in {
    val es: ExecutorService = Executors.newCachedThreadPool()
    val s: Par[List[Int]] = sequence(List(unit(1), unit(2)))
    
    val res: Future[List[Int]] = s(es)
    res.get should be(List(1,2))
  }
}
