package util.parsercomb

case class ParseErr(
    stk: List[(Location, String)] = Nil,
    failures: List[ParseErr] = Nil
):
  def push(loc: Location, msg: String): ParseErr =
    copy((loc, msg) :: stk)

  def latest: Option[(Location, String)] = stk.lastOption

  def label(s: String): ParseErr =
    // replace string in latest w/ given label
    ParseErr(latest match {
      case None       => Nil
      case Some(l, _) => List((l, s))
    })

  def stack: String =
    stk.map((loc, msg) => s"[${loc.line}]: $msg").mkString("\n")

  def errorMessage = latest.map(_._2).getOrElse("")
  def errorLocation = latest.map(_._1).getOrElse(Location("", 0))

  def simplifyStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1)
      .view
      .mapValues(_.map(_._2).mkString("; "))
      .toList
      .sortBy(_._1.offset)

  def addFailure(e: ParseErr) =
    copy(stk = stk ++ e.stk)
  override def toString: String =
    if stk.isEmpty then "no error msg"
    else
      val stack = simplifyStack(stk)
      val loc = stk.lastOption.map(_._1).getOrElse(Location("", 0))
      val caret = " " * (loc.col - 1) + "^"
      s"\n${loc.currentLine}"
        + s"\n$caret"
        + stack
          .map((loc, msg) => s"[${loc.line}.${loc.col}]: $msg")
          .mkString("\n", "\n", "\n")
        + "with other failures:\n"
        + failures.toString

    // stk
    //   .map((loc, msg) =>
    //     s"[row ${loc.line}, col ${loc.col}] (${loc.currentLine}): $msg"
    //   )
    //   .mkString("\n")
