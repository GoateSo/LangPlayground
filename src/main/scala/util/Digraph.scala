package util
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

// bit of imperative code here but alas its still a weightless digraph implementation
class Digraph[T]:
  private var adjList = HashMap[T, List[T]]()

  def +=(pair: (T, T)): Unit =
    val (from, to) = pair
    val rest = apply(from)
    adjList(from) = to :: rest

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

  override def toString(): String = adjList
    .map { (from, tos) => s"${from} -> [${tos.mkString(", ")}]" }
    .mkString("\n")
