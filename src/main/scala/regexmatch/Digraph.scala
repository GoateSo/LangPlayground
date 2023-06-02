package regexmatch
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

// bit of imperative code here but alas its still a weightless digraph implementation
class Digraph[T]:
  private var adjList = HashMap[T, List[T]]()

  // adds edge to graph
  def +=(pair: (T, T)): Unit =
    val (from, to) = pair
    val rest = apply(from)
    adjList(from) = to :: rest

  // gets adjacent nodes at given edge
  def apply(from: T): List[T] = adjList.getOrElse(from, List.empty)

  def dfs(starts: T*): HashSet[T] =
    val visited = HashSet[T]()
    def dfsHelper(froms: T*): Unit =
      for from <- froms do
        visited += from
        val neighbors = apply(from).filterNot(visited.contains)
        for to <- neighbors do if !visited.contains(to) then dfsHelper(to)
    dfsHelper(starts: _*)
    visited

  // def disp[U](f : T => U) : Unit =
  //   adjList.foreach { case (from, tos) =>
  //     println(s"${f(from)} -> [${tos.map(f).mkString(", ")}]")
  //   }
