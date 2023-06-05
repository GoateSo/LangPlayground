package regexmatch
import util.FunDigraph

class FunRegexMatcher(reg: String):
  private def assert(b: Boolean, msg: String = "bad"): Unit =
    if !b then throw Exception(msg)

  // si : search index     | is ind of ] if chrset
  // bi : backtrack index  | is ind of [ in chrset and = si otherwise
  // lp : last paren index
  private def procGlob(si: Int, bi : Int, lp: Int): List[(Int, Int)] =
    if si >= reg.length - 1 then Nil
    else
      // println((si, bi))
      // println((bi + 1, lp))
      reg(si + 1) match
        case '*' => List((lp, si + 1), (si + 1, lp)) // self loop
        case '+' => List((si + 1, lp)) // epsilon transition backward
        case '?' => List((lp, si + 1)) // epsilon transition forward
        case _   => Nil

  enum CharType:
    case Literal(x: Char)
    case Range(x: Char, y: Char)
    case Negated(body: List[CharType])

  import CharType._

  private def parseLit(chrs: List[Char]): (Option[Literal], List[Char]) = chrs match
    case Nil => (None, Nil)
    case '\\' :: Nil => throw Exception("bad?")
    case '\\' :: c :: cs => (Some(Literal(c)), cs)
    case c :: cs if c == '-' || c == ']' => (None, chrs)
    case c :: cs => (Some(Literal(c)), cs)

  private def parseCharSet(i: Int, e: Int): List[CharType] =
    val chrs = reg.substring(i, e).toList
    // negate contents if first char is ^
    val (neg, chrs2) =
      if chrs.head == '^' then (true, chrs.tail) else (false, chrs)
    // parse char set body
    val body = parseCharSetBody(chrs2)
    // negate body if needed
    if neg then List(Negated(body)) else body

  def parseCharSetBody(chrs: List[Char]): List[CharType] =
    val (ch, rest) = parseLit(chrs)
    ch match
      case None =>
        rest match
          case Nil       => throw Exception("unclosed char set")
          case ']' :: cs => assert(cs.isEmpty); Nil
          case c :: cs   => throw Exception("should be unreachable")
      case Some(Literal(x)) =>
        rest match // Literal(x) :: parseCharSetBody(rest)
          // followed by '-' => char range
          case ']' :: cs => assert(cs.isEmpty); Literal(x) :: Nil
          case '-' :: cs =>
            val (ch2, rest2) = parseLit(cs)
            ch2 match
              case None             => throw Exception("bad?")
              case Some(Literal(y)) => Range(x, y) :: parseCharSetBody(rest2)
          // followed literal char, or smth that should be read as literal
          case _ => Literal(x) :: parseCharSetBody(rest)

  private def chrMatch(chr: Char, chrSet: List[CharType]): Boolean =
    chrSet.exists {
      case Literal(x)  => x == chr
      case Range(x, y) => x <= chr && chr <= y
      case Negated(xs) => !chrMatch(chr, xs)
    }
  private def process(
      i: Int,
      graph: FunDigraph[Int],
      stk: List[Int],
      chrSets: Map[Int, (List[CharType], Int)]
  ): (FunDigraph[Int], Map[Int, (List[CharType], Int)]) =
    if i >= reg.length then (graph, chrSets)
    else
      val c = reg(i); assert(c != ']', "unmatched brackets in regex")
      // handle groups and embedded alternations
      val (lp, g, stk2) =
        if c == '(' || c == '|' then (i, graph, i :: stk)
        else if c == ')' then
          assert(stk.exists(reg(_) == '('), "unmatched parens in regex")
          val (ors, lp :: rest) = stk.span(reg(_) != '('): @unchecked
          (lp, graph ++ ors.flatMap(or => List((lp, or + 1), (or, i))), rest)
        else (i, graph, stk)
      val (nind, nchrsets) = if c == '[' then
        val j = reg.indexOf(']', i)
        assert(j > 0, "unmatched brackets in regex")
        // correspond starting index with charSet obj and next index
        (j, chrSets.updated(i, (parseCharSet(i + 1, j + 1), j + 1)))
      else (i, chrSets)
      // handle globs (kleene star, plus, and optional)
      val graph2 = g ++ procGlob(nind, i, lp)
      // special char epsilon transitions
      val graph3 = c match
        case '(' | '*' | ')' | '?' | '+' => graph2 + (i, i + 1)
        case _                           => graph2
      process(nind + 1, graph3, stk2, nchrsets)

  val (digraph, chrSets) = process(
    0,
    FunDigraph(Map.empty),
    List.empty,
    Map.empty // reg.length might also work :p
  )

  val x = digraph.toString()

  def matches(input: String, partial: Boolean = false): Boolean =
    def matchHelper(i: Int, vset: Set[Int]): Boolean =
      // println(("a", i, vset))
      if i == input.length then vset.contains(reg.length)
      else if partial && vset.contains(reg.length) then true
      else
        val next = vset
          .withFilter(v => v < reg.length)
          .withFilter(v =>
            if reg(v) == '.' then true
            else if chrSets.contains(v) then chrMatch(input(i), chrSets(v)._1)
            else reg(v) == input(i)
          )
          .map(v => if chrSets.contains(v) then chrSets(v)._2 else v + 1)
        if next.isEmpty then false
        else matchHelper(i + 1, digraph.dfs(next.toList: _*).toSet)
    matchHelper(0, digraph.dfs(0).toSet)

  def finds(input: String): Boolean =
    (0 until input.length) exists { i =>
      matches(input.substring(i), true)
    }
