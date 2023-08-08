package strsearch

/** generic interface for string search algorithms
  *
  * @param pattern
  */
trait StringSearch(pattern: String):
  def indexIn(queryStr: String, start: Int = 0): Int

  /** checks if the pattern is contained in the given string
    *
    * @param queryStr
    *   the string to search in
    * @return
    *   true if the pattern is contained in the string, false otherwise
    */
  def contains(queryStr: String): Boolean = indexIn(queryStr) != -1
