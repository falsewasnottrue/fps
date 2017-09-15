package ch3

sealed trait Tree[+A]
case class Leaf[A](a: A) extends Tree[A]
case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]

object Tree {

  // 3.25 size
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  // 3.26 maximum
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(m) => m
    case Branch(l,r) => Math.max(maximum(l), maximum(r))
  }

  // 3.27 depth
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + Math.max(depth(l), depth(r))
  }
}
