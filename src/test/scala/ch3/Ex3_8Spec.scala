package ch3

object Ex3_8Spec extends App {
  val arg: List[Int] = List(1,2,3)
  val result: List[Int] = List.foldRight(arg, Nil:List[Int])(Cons(_, _))

  println(arg)
  println(result)
  println(arg == result)
}
