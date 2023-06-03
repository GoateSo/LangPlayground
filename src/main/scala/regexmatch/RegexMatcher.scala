package regexmatch
import scala.util.boundary.*
// to add:
// ?           : 0 or 1 of something (done)
// +           : 1 or more of something (done)
// (a | b | c) : multi alternation (done)
// [abc]       : char set
// [a-z]       : charset (range)

import scala.collection.mutable.{Stack, HashSet, HashMap}
import scala.util.boundary
// implements a regex matching function for just the operations of
// concatenation (ab), alternation (a|b), and Kleene star (a*), and the groupings ((a)) for precedence.
// creates a digraph from the regex and then uses a depth first search to find a match
// implementation from Algorithsm 4th edition
class RegexMatcher(reg: String) {
  // maintain digraph of indices, with edges representing epsilon transitions
  // if adding charSets: make digraph into Digraph[String] and change processing /  match accordingly
  val digraph = new Digraph[Int]()
  val stack = Stack[Int]()

  // parse using stack like shunting yard
  // keep track of index
  for i <- 0 until reg.length do
    var lp = i
    // step 1: deal w/ groups and embedded alternations
    // if its left paren or alternation push to stack
    if reg(i) == '(' || reg(i) == '|' then stack.push(i)
    // if its right parenm, check if top is alt, and if so find the next (real) left paren and add to digraph
    // assumption: no statement in form (a|b|c|...)
    else if reg(i) == ')' then
      assert(stack.nonEmpty, "unmatched parens in regex")
      val xs = scala.collection.mutable.ListBuffer[Int]()
      while !stack.isEmpty && reg(stack.top) != '(' do
        xs += stack.pop
      assert(reg(stack.top) == '(')
      lp = stack.pop
      for or <- xs do 
        digraph += (lp, or+1)
        digraph += (or, i)

    // step 2 check for kleene star with a lookahead and add a self loop
    if (i < reg.length - 1 && reg(i + 1) == '*') then
      digraph += (i + 1, lp)
      digraph += (lp, i + 1)
    // step 2.5 check for ? and add epsilon transition forward only
    if (i < reg.length - 1 && reg(i + 1) == '?') then 
      digraph += (lp, i + 1)
    // step 2.75 check for + and add epsilon transition backward only
    if (i < reg.length - 1 && reg(i + 1) == '+') then 
      digraph += (i + 1, lp)
    // step 3: add forward edge to next char for special chars
    if reg(i) == '(' || reg(i) == '*' || reg(i) == ')' 
    || reg(i) == '?' || reg(i) == '+'
    then digraph += (i, i + 1)

  val x = digraph.toString()

  // checks if a string wholly matches the regular expression
  def matches(input : String, partial : Boolean = false) : Boolean =
    // initial set of vertices reachable from start
    var visSet = digraph.dfs(0)
    // for each vertex reachable, check if it matches the next char in the input
    boundary:
      for i <- 0 until input.length do
        visSet.filterInPlace(v => v < reg.length && (reg(v) == input(i) || reg(v) == '.'))
        visSet = digraph.dfs(visSet.toList.map(_ + 1) : _*)
        if visSet.isEmpty then boundary.break(false)
        // if partial match is allowed, check if end is reachable
        if partial && visSet.contains(reg.length) then boundary.break(true)
      // check if end is reachable
      visSet.contains(reg.length)

  // check if a substring of the input matches the regular expression
  def finds(input: String): Boolean =
    (0 until input.length) exists { i =>
      matches(input.substring(i), true)
    }
}
