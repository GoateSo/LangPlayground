package util.parsercomb.exparsers
import util.parsercomb.*
import scala.languageFeature.implicitConversions

object JsonParser:
  enum JSON:
    case JNull
    case JNum(get: Double)
    case JStr(get: String)
    case JBoo(get: Boolean)
    case JArr(get: IndexedSeq[JSON])
    case JObj(get: Map[String, JSON])
  end JSON

  def parse[Err, Parser[+_]](P: Parsers[Parser]): Parser[JSON] =
    import JSON.*
    import P.*

    given Conversion[String, Parser[String]] = string(_).token

    def ident: Parser[String] =
      (cond(_.isLetter) ** cond(_.isLetterOrDigit).many map ((a, b) =>
        a :: b
      )).slice

    def nul = (("null" | "undefined") map (_ => JNull))
      .scope("json null")

    def fractional = (char('.') ** digits).maybe map (a =>
      a map ((_, frac) => f"0.${frac}".toDouble) getOrElse 0d
    )

    def num = (digits.map2(fractional)(_ + _).token map JNum.apply)
      .scope("json number")

    // very monke yes, doesnt take into account escaped strings
    def str = char('"') ** cond(_ != '"').many.slice ** char('"') map {
      case ((q1, body), q2) => body
    }

    def jstr = str.map(JStr(_)).scope("json string")

    def bool =
      ("true".as(JBoo(true)) | "false".as(JBoo(false)))
        .scope("json bool")

    def literal = (nul | num | jstr | bool) scope "json literal"

    def array: Parser[JSON] =
      ("[" *> value.sep(",").map(_.toIndexedSeq) <* "]") map {
        JArr(_)
      } scope "json array"

    def objEntry: Parser[(String, JSON)] =
      (((ident | str).token <* ":") ** value) scope "json object entry"

    def obj: Parser[JSON] =
      ("{" *> objEntry.sep(",") <* "}") map { xs =>
        JObj(xs.groupMapReduce(_._1)(_._2)((a, b) => b))
      } scope "json object"

    def value: Parser[JSON] = literal | array | obj

    value
  end parse
