package util.parsercomb.exparsers
import util.parsercomb.Parsers
import scala.languageFeature.implicitConversions
object XMLParser {

  enum XML:
    case Elem(name: String, attrs: Map[String, String], children: List[XML])
    case Text(value: String)
  def mkParser[PErr, Parser[+_]](P: Parsers[Parser]) =
    import P.*
    import XML.*

    given Conversion[String, Parser[String]] = string(_).token

    def ident: Parser[String] = regex("[a-zA-Z][a-zA-Z0-9]*".r)

    def quoted: Parser[String] =
      (char('"') *> cond(_ != '"').many.slice <* char('"')).scope("quoted")

    def attr: Parser[(String, String)] =
      (ident.token
        <* "=".commitTo)
        ** quoted

    def attrs: Parser[Map[String, String]] =
      attr
        .sep(whitespace)
        .map(_.toMap) scope "attributes"

    def elem: Parser[XML] =
      (for {
        (id, attrs) <-
          ("<" *> (ident.token ** attrs) <* ">")
            .label("unable to parse opening tag") scope "opening tag"
        children <- xml.many scope s"$id's body"
        // context sensitive closing tag
        _ <- ("</" ** token(id) ** ">") label s"Expected closing tag for <$id>"
      } yield Elem(id, attrs, children)) scope "element"

    def text: Parser[XML] =
      cond(_ != '<', "not '<'").many1.slice.map(Text(_)) scope "text"

    def xml: Parser[XML] = elem.attempt | text

    elem
}
