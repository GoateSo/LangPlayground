package strsearch
import scala.util.boundary

class BoyerMoore(pat: String) extends StringSearch(pat):

  val right = Array.ofDim[Int](256)
  // initalize array of rightmost character occurences
  for c <- 0 until 256 do right(c) = -1
  for j <- 0 until pat.length do right(pat(j)) = j

  override def indexIn(queryStr: String, start: Int): Int =
    var skip = 0
    val qlen = queryStr.length
    val plen = pat.length
    boundary:
      var i = start
      while i <= qlen - plen do
        // does pattern match text at at position i?
        skip = 0 
        boundary:
          for j <- plen - 1 to 0 by -1 do
            if pat(j) != queryStr(i+j) then   // mismatch
              // if char is not found in pattern: skip = j - (-1) = j + 1
              // if char is found in pattern: skip = j - right(queryStr(i+j))
              //    that is, offsetting the pattern so that the char aligns
              //    with its rightmost occurence in the pattern
              skip = j - right(queryStr(i+j)) 
              if skip < 1 then skip = 1
              boundary.break() 
        // no mismatch found, return index
        if skip == 0 then boundary.break(i)
        i += skip
      // exited without finding a match
      -1
      

