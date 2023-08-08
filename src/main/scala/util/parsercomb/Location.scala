package util.parsercomb

final case class Location(input: String, offset: Int):

  private lazy val portion = input.slice(0, offset + 1)
  lazy val line = portion.count(_ == '\n') + 1
  lazy val col = portion.lastIndexOf('\n') match
    case -1        => offset + 1
    case lineStart => offset - lineStart

  def toError(msg: String): ParseErr = ParseErr(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  def currentLine: String =
    if input.length > 1
    then input.linesIterator.drop(line - 1).next().stripLeading()
    else ""
