package ch7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean =  true
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def isCancelled: Boolean = false
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = pa(es)
      val bf = pb(es)

      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](pa: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = pa(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  // ex 7.4
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
  
  // ex 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ???

  def run[A](s: ExecutorService)(pa: Par[A]): Future[A] = pa(s)
}