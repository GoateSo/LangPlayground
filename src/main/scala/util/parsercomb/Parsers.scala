package util.parsercomb

import scala.util.matching.Regex

trait Parsers[Parser[+_]]:
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def charSet(s: String): Parser[Char] = cond(
    s.contains(_),
    s"Expected one of: [$s]"
  )
  def string(s: String): Parser[String]
  def cond(
      f: Char => Boolean,
      msg: String = s"Failed to match predicate"
  ): Parser[Char]
  def pure[A](a: A): Parser[A]
  def fail(msg: String): Parser[Nothing]
  def regex(r: Regex): Parser[String]

  def digits = cond(_.isDigit, "isn't digit").many1.slice.map(_.toInt)
  def whitespace = cond(_.isWhitespace, "isn't whitespace").many.slice
  // ---- error handling ----

  extension [A](p: Parser[A])
    def run(input: String): Either[ParseErr, A]

    def label(s: String): Parser[A]
    def scope(s: String): Parser[A]

    // primitives for commitment
    def attempt: Parser[A]
    // obeys following rule:
    // attempt (p flatmap (_ => fail(...))) or p2 ~> p2 (w/ some added error info)
    // left biased union assumption
    infix def or(p2: => Parser[A]): Parser[A]
    inline def |(p2: => Parser[A]): Parser[A] = p.or(p2)

    def slice: Parser[String]

    def many: Parser[List[A]] =
      p.map2(p.many)(_ :: _) | pure(Nil)

    def maybe: Parser[Option[A]] =
      p.map(Some(_)).attempt | pure(None)

    def listOfN(n: Int): Parser[List[A]] =
      if n <= 0
      then pure(Nil)
      else p.map2(p.listOfN(n - 1))(_ :: _)

    def sep(separator: Parser[Any]): Parser[List[A]] =
      sep1(separator) | pure(Nil)

    def sep1(separator: Parser[Any]): Parser[List[A]] =
      p.map2((separator *> p).many)(_ :: _)

    def token: Parser[A] =
      p <* whitespace

    // exercise 9.8
    def map[B](f: A => B): Parser[B] =
      p.flatMap(f andThen pure)

    def <*(p2: => Parser[Any]): Parser[A] =
      p.map2(p2.slice)((a, _) => a)

    def *>[B](p2: => Parser[B]): Parser[B] =
      p.slice.map2(p2)((_, b) => b)

    // exercise 9.7
    def and[B](p2: => Parser[B]): Parser[(A, B)] =
      p flatMap (a => p2.map(b => (a, b)))
    inline def **[B](p2: => Parser[B]): Parser[(A, B)] = p and p2

    // exercise 9.1:
    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      (p ** p2).map(f.tupled)
      // or in terms of flatmap: p ** p2 flatMap (f.tupled andThen pure)

    def many1: Parser[List[A]] =
      p.map2(p.many)(_ :: _)

    def as[B](b: B): Parser[B] =
      p.map(_ => b)

    // 9.2 laws for product(and): semigroup/ monoid rules??
    // 1. run((p1 ** p2) ** p3)(str) <~> run(p1 ** (p2 ** p3))
    // 2. run(string("") ** p)(str) <~> run(p)(str) <~> run(p ** string(""))

    def flatMap[B](f: A => Parser[B]): Parser[B]
    inline def >>=[B](f: A => Parser[B]): Parser[B] = flatMap(f)

    def withFilter(f: A => Boolean): Parser[A] =
      p.flatMap(a => if f(a) then pure(a) else fail(""))

    def commitTo: Parser[A]
  end extension
