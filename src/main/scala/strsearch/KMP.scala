package strsearch

import scala.collection.mutable.ArraySeq

/** Knuth-Morris-Pratt algorithm for substring search
  *
  * @param pattern
  *   the pattern to search for
  */
class KMP(val pattern: String) extends StringSearch(pattern):
  val len = pattern.length
  val dfa = Array.ofDim[Int](256, len)

  // build DFA
  // inital transition
  dfa(pattern(0))(0) = 1
  var x = 0 // restart state
  for j <- 1 until len do
    // copy mismatch cases
    for c <- 0 until 256 do dfa(c)(j) = dfa(c)(x)
    dfa(pattern(j))(j) = j + 1 // set match case
    x = dfa(pattern(j))(x) // restart state update

  /*
    DFA building works because for string w/ chars say [c1,...,c11] and pattern [p1,...,p8]
    matching like so w/ a mismatch:

      c1 c2 c3  c4 c5 c6 c7  |c8| c9 c10 c11
            p1 [p2 p3 p4 p5] |p6| p7 p8
        next:  [p1 p2 p3 p4]  p5  p6 p7 p8
               ^ overlapping segment

    in finding the next fallback before proceding, we know that c3=p1, c4=p2, ..., c7=p5.
    so then we can avoid going back to the beginning by finding a suffix of the subpattern
    [px,...,p5] that matches a prefix of the pattern [p1,p2,p3,p4,p5,p6,p7,p8] (x >= 2),
    and then going the end of that pattern to begin the next search.

    to do this we'd run a parallel simulation of the DFA, but instead of starting at the
    first char of the pattern, we start at the second char of the pattern, creating a subpattern
    [p2,...,pi] for every 2 <= i <= 8, and then using that to determine the fallback indices

   */
  /** searches for the pattern in the given string
    *
    * @param queryStr
    *   the string to search in
    * @param start
    *   the starting index in the string to search from
    * @return
    *   starting index of the pattern in the string, -1 if not found
    */
  override def indexIn(queryStr: String, start: Int = 0): Int =
    var i = start // index in queryStr
    var j = 0 // index in pattern
    while i < queryStr.length && j < len do
      // simulate DFA (find next index in pattern)
      j = dfa(queryStr(i))(j)
      // advance index in queryStr
      i += 1
    // if pattern is fully matched, return starting index, otherwise -1
    if j == len then i - len else -1
