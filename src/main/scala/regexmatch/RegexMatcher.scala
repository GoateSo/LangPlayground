package regexmatch
import scala.collection.mutable.{Stack, HashSet, HashMap}
// implements a regex matching function for just the operations of 
// concatenation (ab), alternation (a|b), and Kleene star (a*), and the groupings ((a)) for precedence.
// creates a digraph from the regex and then uses a depth first search to find a match
// implementation from Algorithsm 4th edition
class RegexMatcher(reg : String) {
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
    if reg(i) == '(' || reg(i) == '|' then 
      stack.push(i)
    // if its right parenm, check if top is alt, and if so find the next (real) left paren and add to digraph
    // assumption: no statement in form (a|b|c|...)
    else if reg(i) == ')' then
      val or = stack.pop()
      if reg(or) == '|' then
        lp = stack.pop()
        digraph += (lp, or + 1)
        digraph += (or, i)
      else lp = or
    // step 2 check for kleene star with a lookahead and add a self loop
    if (i < reg.length - 1 && reg(i + 1) == '*') then 
      digraph += (i + 1, lp)
      digraph += (lp, i + 1)
    // step 3: add forward edge to next char for special chars 
    if reg(i) == '(' || reg(i) == '*' || reg(i) == ')' then
      digraph += (i, i + 1)

  digraph.disp(i => (if i == reg.length then "end" else reg(i).toString, i))


  // checks if a string wholly matches the regex
  def matches(input : String) : Boolean = 
    // initial set of vertices reachable from start
    var visSet = digraph.dfs(0)
    // for each vertex reachable, check if it matches the next char in the input
    for i <- 0 until input.length do
      var matchSet = HashSet[Int]()
      for v <- visSet do
        // char matches or is wildcard, advancing to next index (non epsilon) works as path
        if v < reg.length && (reg(v) == input(i) || reg(v) == '.') then
          matchSet += v + 1
      visSet = digraph.dfs(matchSet)
    // check if end is reachable
    visSet.contains(reg.length)
}
