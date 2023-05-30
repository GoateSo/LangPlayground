package testparser1; 

object Tokenizer:
  // table of special characters
  val schars = Set(
    ',', '$', '=', '(', ')', '{', '}', '[', ']', ';', '\"', '\''
  )

  // table of operators
  val ops = Set(
    '+', '-', '*', '/', '%', '^', '&', '|', '~', '!', '<', '>'
  )

  private enum State:
    case Empty, Ident, Num, Op
  import State.*

  enum Token:
    case Ident(value: String)
    case Num(value: Double)
    case Op(value: String)
  // import Token.*

  private def isSpecial(c: Char) =
    ops(c) || schars(c)

  // replace (State, Char, String) with (State, String, String) to make it match the lua impl
  private def process(
      state: State,
      c: Char,
      cval: String
  ): (State, String, String) = state match
    case Empty =>
      if c.isLetter || c == '_' then (Ident, s"$c", cval)
      else if c.isDigit then (Num, s"$c", cval)
      else if isSpecial(c) then (Op, s"$c", cval)
      else if c.isWhitespace then (Empty, s"$c", cval)
      else throw Exception("Invalid character after empty state: " + c)
    case Ident =>
      if c.isLetter || c.isDigit || c == '_' then (Ident, cval + c, "")
      else if isSpecial(c) then (Op, s"$c", cval)
      else if c.isWhitespace then (Empty, s"$c", cval)
      else
        println(cval)
        throw Exception(
          s"Invalid character after ident state: ($c)"
        ) // TODO - different from lua impl, maybe make match?
    case Num =>
      if c.isDigit || c == '.' then (Num, cval + c, "")
      else if c.isWhitespace then (Empty, s"$c", cval)
      else if isSpecial(c) then (Op, s"$c", cval)
      else if c.isLetter || c == '_' then (Ident, s"$c", cval)
      else throw Exception("Invalid character after num state: " + c)
    case Op =>
      if c.isDigit || c == '.' then (Num, s"$c", cval)
      else if isSpecial(c) then (Op, s"$c", cval)
      else if c.isLetter || c == '_' then (Ident, s"$c", cval)
      else (Empty, s"$c", cval)

  private def getToken(state: State, cval: String): Token = state match
    case Empty =>
      throw Exception("Empty state should not have a value")
    case Ident => Token.Ident(cval)
    case Num   => Token.Num(cval.toDouble)
    case Op    => Token.Op(cval)

  private def m_Tokenize(
      str: List[Char],
      curval: String = "",
      state: State = Empty,
      cur: List[Token] = Nil
  ): List[Token] = str match
    case Nil =>
      getToken(state, curval) :: cur
    case c :: cs =>
      val (nstate, ncur, nval) = process(state, c, curval)
      if !nval.isEmpty() && nval.exists(!_.isWhitespace)
      then
        m_Tokenize(
          cs,
          ncur,
          nstate,
          getToken(state, nval) :: cur
        )
      else m_Tokenize(cs, ncur, nstate, cur)

  def tokenize(str: String): List[Token] =
    m_Tokenize(str.strip.toList).reverse
