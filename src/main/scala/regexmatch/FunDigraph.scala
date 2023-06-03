package regexmatch

class FunDigraph[T](private val repr: Map[T, List[T]]):

  def apply(from: T) = repr.getOrElse(from, List.empty)

  def + (pair: (T, T)): FunDigraph[T] =
    val (from, to) = pair
    val rest = apply(from)
    new FunDigraph(repr + (from -> (to :: rest)))
  
  def ++(entries: (T,T)*): FunDigraph[T] = 
    entries.foldLeft(this)(_ + _)

  def dfs(starts: T*): Set[T] =
    def dfsHelper(visited: Set[T], node: T): Set[T] =
      if visited.contains(node)
      then visited
      else apply(node).foldLeft(visited + node)(dfsHelper)
    starts.foldLeft(Set.empty[T])(dfsHelper)

  override def toString(): String = repr
    .map { (from, tos) => s"${from} -> [${tos.mkString(", ")}]" }
    .mkString("\n")
