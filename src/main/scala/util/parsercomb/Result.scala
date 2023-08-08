package util.parsercomb

enum Result[+A]:
  case Success(get: A, consumed: Int)
  case Failure(get: ParseErr, commited: Boolean)
  // 'left map' of softs
  def mapError(f: ParseErr => ParseErr): Result[A] = this match
    case Failure(get, c) => Failure(f(get), c)
    case _               => this

  // def map[B](f: A => B): Result[B] = this match
  //   case Success(get, consumed) => Success(f(get), consumed)
  //   case Failure(get, commited) => Failure(get, commited)

  def advanceSuccess(n: Int): Result[A] = this match
    case Success(get, consumed) => Success(get, consumed + n)
    case _                      => this

  def uncommit: Result[A] = this match
    case Failure(get, true) => Failure(get, false)
    case _                  => this

  def addCommit(commited: Boolean): Result[A] = this match
    case Failure(get, false) => Failure(get, commited)
    case _                   => this
