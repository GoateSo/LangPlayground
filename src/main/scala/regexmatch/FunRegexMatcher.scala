package regexmatch

class FunRegexMatcher(reg: String):
  private def process(
      i: Int,
      cur: FunDigraph[Int],
      stk: List[Int]
  ): FunDigraph[Int] =
    if i >= reg.length then cur
    else
      val c = reg(i)
      val (lp, g, stk2) =
        if c == '(' || c == '|' then (i, cur, i :: stk)
        else if reg(i) == ')' then
          val or :: next = stk : @unchecked
          if reg(or) == '|' then
            val lp :: rest = next : @unchecked
            (lp, cur + (lp, or + 1) + (or, i), rest)
          else (or, cur, next)
        else (i, cur, stk)
      val cur2 =
        if i < reg.length - 1 then
          reg(i + 1) match
            case '*' => g + (lp, i + 1) + (i + 1, lp)
            case '+' => g + (i + 1, lp)
            case '?' => g + (lp, i + 1)
            case _   => g
        else g
      val cur3 =
        if c == '(' || c == ')' || c == '*' || c == '+' || c == '?'
        then cur2 + (i, i + 1)
        else cur2
      process(i + 1, cur3, stk2)

  val digraph = process(0, FunDigraph(Map.empty), List.empty)

  def matches(input: String, partial: Boolean = false): Boolean =
    def matchHelper(i: Int, vset: Set[Int]): Boolean =
      if i == input.length then vset.contains(reg.length)
      else if partial && vset.contains(reg.length) then true
      else
        val next = vset.filter(v =>
          v < reg.length && (reg(v) == input(i) || reg(v) == '.')
        )
        if next.isEmpty then false
        else matchHelper(i + 1, digraph.dfs(next.toList.map(_ + 1): _*).toSet)
    matchHelper(0, digraph.dfs(0).toSet)

  def finds(input: String): Boolean =
    (0 until input.length) exists { i =>
      matches(input.substring(i), true)
    }