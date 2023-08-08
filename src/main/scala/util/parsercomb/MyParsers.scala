package util.parsercomb

import scala.util.matching.Regex
import Result.*
import scala.collection.mutable.ListBuffer
import scala.util.boundary

type Parser[+A] = Location => Result[A]

object MyParsers extends Parsers[Parser]:

  override def fail(msg: String): Parser[Nothing] = _ =>
    Failure(Location("", 0).toError(msg), true)

  def cond(
      pred: Char => Boolean,
      msg: String = s"Failed to match predicate"
  ): Parser[Char] = { case l @ Location(input, offset) =>
    if offset < input.length && pred(input(offset))
    then Success(input(offset), 1)
    else
      Failure(
        l.toError(
          msg + s" | queried char: '${
              if offset < input.length then input(offset) else "<EOF>"
            }'"
        ),
        false
      )
  }

  override def char(c: Char): Parser[Char] = {
    case l @ Location(input, offset) =>
      if offset < input.length && input(offset) == c
      then Success(c, 1)
      else Failure(l.toError(s"expected '$c'"), false)
  }

  private def firstMismatchInd(s1: String, s2: String, offset: Int): Int =
    var i = 0 
    while i + offset < s1.length && i < s2.length do
      if s1(i + offset) != s2(i) then return i
      i += 1
    if s1.length - offset < s2.length then i else -1
  def string(s: String): Parser[String] =
    val parser: Parser[String] = { case l @ Location(input, offset) =>
      val ind = firstMismatchInd(input, s, offset)
      if ind == -1
      then Success(s, s.length)
      else Failure(l.toError(""), ind != 0)
    }
    parser.label(s"Expected: \"$s\"")

  def regex(r: Regex): Parser[String] = { case l @ Location(input, offset) =>
    if offset >= input.length
    then Failure(l.toError(s"Expected regex: $r"), false)
    else r.findPrefixOf(input.substring(offset)) match
        case Some(value) => Success(value, value.length)
        case None =>
          Failure(l.toError(s"Failed to match $r"), false)
  }

  def pure[A](a: A): Parser[A] = _ => Success(a, 0)

  extension [A](p: Parser[A])
    def slice: Parser[String] = { case Location(input, offset) =>
      p(Location(input, offset)) match
        case Success(_, consumed) =>
          Success(input.slice(offset, offset + consumed), consumed)
        case Failure(err, isCom) => Failure(err, isCom)
    }

    // attempt but don't lock in on failure
    def attempt: Parser[A] = l => p(l).uncommit

    // tries left parser, if it fails w/o commit, tries right parser
    override def or(p2: => Parser[A]): Parser[A] = l =>
      p(l) match
        case Failure(get, false) => p2(l).mapError(_.addFailure(get))
        case a                   => a

    // overriden many impl that uses a loop and listbuffer
    // to avoid stack overflow
    override def many: Parser[List[A]] = loc =>
      val lbuff = ListBuffer[A]()
      var offset = 0
      boundary:
        while true do
          p(loc.advanceBy(offset)) match
            case Success(get, consumed) =>
              lbuff += get
              offset += consumed
            case Failure(get, true)  => 
              boundary.break(Failure(get, true))
            case Failure(get, false) => 
              boundary.break(Success(lbuff.toList, offset))
        Success(lbuff.toList, offset)
    def scope(s: String): Parser[A] = loc => p(loc).mapError(_.push(loc, s))

    def label(s: String): Parser[A] = loc => p(loc).mapError(_.label(s))

    def flatMap[B](f: A => Parser[B]): Parser[B] = 
      loc =>
        p(loc) match // if successful, procede with next parser at next location
          case Success(get, consumed) =>
            f(get)(loc.advanceBy(consumed))
              .addCommit(consumed != 0)
              .advanceSuccess(consumed)
          case Failure(get, iscommited) =>
            Failure(get, iscommited)

    override def and[B](p2: => Parser[B]): Parser[(A, B)] = loc =>
      p(loc) match
        case Success(a, c1) =>
          p2(loc.advanceBy(c1)) match
            case Success(b, c2)  => Success((a, b), c1 + c2)
            case Failure(m, com) => Failure(m, com).addCommit(c1 != 0)
        case Failure(get, commited) => Failure(get, commited)

    override def map[B](f: A => B): Parser[B] = loc =>
      p(loc) match
        case Success(get, consumed) => Success(f(get), consumed)
        case Failure(get, commited) => Failure(get, commited)

    override def run(input: String): Either[ParseErr, A] = p(
      Location(input, 0)
    ) match
      case Success(get, _) => Right(get)
      case Failure(get, _) => Left(get)

    def commitTo: Parser[A] = loc => p(loc) match
      case Failure(get, commited) => Failure(get, true)
      case a                      => a
    
end MyParsers
