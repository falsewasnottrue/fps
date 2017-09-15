package ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](ls: List[A], h: A): List[A] = ls match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def drop[A](ls: List[A], n: Int): List[A] =
    if (n==0) ls else drop(List.tail(ls), n-1)

  def dropWhile[A](ls: List[A])(p: A => Boolean): List[A] = ls match {
    case Cons(h, t) if p(h) => dropWhile(t)(p)
    case _ => ls
  }

  def init[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](ls: List[A], z: B)(f: (A,B) => B): B = ls match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t,z)(f))
  }

  def sum2(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)
  def product2(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  def length[A](ls: List[A]): Int = foldRight(ls, 0)((_, l) => l+1)

  @annotation.tailrec
  def foldLeft[A,B](ls: List[A], z: B)(f: (B,A) => B): B = ls match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // 3.11 sum, product, length via foldLeft
  def sum3(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  def product3(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  def length2[A](ls: List[A]): Int = foldLeft(ls, 0)((l, _) => l + 1)

  // 3.12 reverse
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((l, h) => Cons(h, l))

  // 3.13 foldLeftViaFoldRight & foldRightViaFoldLeft
  def foldLeftViaFoldRight[A,B](ls: List[A], z: B)(f: (B,A) => B): B =
    foldRight(reverse(ls), z)((a, b) => f(b,a))

  def foldRightViaFoldLeft[A,B](ls: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(ls), z)((b,a) => f(a,b))

  // 3.14 append via foldRight
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l,r)(Cons(_,_))

  // 3.15 concat
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())(List.appendViaFoldRight)

  // 3.18 map
  def map[A, B](l: List[A])(f: A => B): List[B] =
    List.foldRight(l, List[B]())((a, t) => Cons(f(a), t))

  // 3.19 removeOdds
  def removeOdds(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) if h % 2 == 1 => removeOdds(t)
    case Cons(h, t) => Cons(h, removeOdds(t))
  }

  // 3.20 flatMap
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // 3.22 addPairwise
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2,addPairwise(t1,t2))
    case _ => Nil
  }

  // 3.23 zipWith
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a,b) match {
    case (Cons(a1, t1), Cons(b1, t2)) => Cons(f(a1,b1), zipWith(t1,t2)(f))
    case _ => Nil
  }


}
