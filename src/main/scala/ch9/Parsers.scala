package ch9

import ch8.Gen

trait Parsers[ParseError, Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def map[A,B](pa: Parser[A])(f: A => B): Parser[B]
  def map2[A,B,C](pa: Parser[A], pb: Parser[B])(f: (A,B) => C): Parser[C] = (pa ** pb).map(f(_))

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def slice[A](p: Parser[A]): Parser[String]
  def many1[A](pa: Parser[A]): Parser[List[A]]
  def product[A,B](pa: Parser[A], pb: Parser[B]): Parser[(A,B)]

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  def many[A](pa: Parser[A]): Parser[List[A]] =
    map2(pa, many(pa))(_ :: _) or succeed(List())
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n==0) succeed(List()) else map2(p, listOfN(n-1, p))(_ :: _)

  implicit def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  implicit def string(s: String): Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  implicit class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
  }

  object Laws {
    import ch8.Prop._

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}
