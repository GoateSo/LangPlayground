package regexmatch

class FunRegexMatcher(reg: String):
  private def process(
      i: Int,
      graph: FunDigraph[Int],
      stk: List[Int]
  ): FunDigraph[Int] =
    if i >= reg.length then graph
    else
      val c = reg(i)
      val (lp, g, stk2) =
        if c == '(' || c == '|' then (i, graph, i :: stk)
        else if reg(i) == ')' then
          val (ors, lp :: rest) = stk.span(reg(_) != '('): @unchecked
          (lp, graph ++ ors.flatMap(or => List((lp, or + 1), (or, i))), rest)
        else (i, graph, stk)
      val graph2 = g
        ++ (if i < reg.length - 1 then
              reg(i + 1) match
                case '*' => List((lp, i + 1), (i + 1, lp))
                case '+' => List((i + 1, lp))
                case '?' => List((lp, i + 1))
                case _   => Nil
            else Nil)
      val graph3 = c match
        case '(' | '*' | ')' | '?' | '+' => graph2 + (i, i + 1)
        case _                           => graph2
      process(i + 1, graph3, stk2)

  val digraph = process(0, FunDigraph(Map.empty), List.empty)

  val x = digraph.toString()

  def matches(input: String, partial: Boolean = false): Boolean =
    def matchHelper(i: Int, vset: Set[Int]): Boolean =
      if i == input.length then vset.contains(reg.length)
      else if partial && vset.contains(reg.length) then true
      else
        val next = vset
          .withFilter(v =>
            v < reg.length && (reg(v) == input(i) || reg(v) == '.')
          )
          .map(_ + 1)
        if next.isEmpty then false
        else matchHelper(i + 1, digraph.dfs(next.toList: _*).toSet)
    matchHelper(0, digraph.dfs(0).toSet)

  def finds(input: String): Boolean =
    (0 until input.length) exists { i =>
      matches(input.substring(i), true)
    }
