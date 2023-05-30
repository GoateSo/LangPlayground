package regexmatch
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

// bit of imperative code here but alas its still a weightless digraph implementation
class Digraph[T]:
  private var adjList = HashMap[T, List[T]]()
  
  // adds edge to graph
  def += (pair : (T, T)) : Unit =
    val (from, to) = pair
    adjList(from) = to :: adjList.getOrElse(from, List.empty)
  
  // gets adjacent nodes at given edge
  def apply(from : T) : List[T] = adjList.getOrElse(from, List.empty)

  def disp[U](f : T => U = identity) : Unit = 
    adjList.foreach { case (from, tos) =>
      println(s"${f(from)} -> [${tos.map(f).mkString(", ")}]")
    }

  def dfs(starts : IterableOnce[T]) : HashSet[T] = 
    val visited = HashSet[T]()
    def dfsHelper(froms : IterableOnce[T]) : Unit = 
      for from <- froms do
        visited += from
        for to <- this(from) do
          if !visited.contains(to) then dfsHelper(List(to))
    dfsHelper(starts)
    visited

  def dfs(start : T) : HashSet[T] = 
    dfs(List(start))  

