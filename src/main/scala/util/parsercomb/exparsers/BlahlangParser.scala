package util.parsercomb.exparsers
import util.parsercomb.*
object TestParser:

  enum Tree:
    case BinOp(op: String, left: Tree, right: Tree)
    case UnOp(op: String, right: Tree)
    case VarDef(name: String, value: Tree)
    case VarMut(name: String, value: Tree)
    case FunDef(name: String, args: List[String], body: Tree)
    case FunCall(name: String, args: List[Tree])
    case Num(value: Double)
    case Ident(name: String)
    case Return(expr: Tree)
    case Program(stmts: List[Tree])
  end Tree

  import Tree.*
  def mkParser[Err, Parser[+_]](P: Parsers[Parser]): Parser[Tree] =
    import P.*
    def token[A](s: String): Parser[String] =
      string(s).token
    def frac = (P.char('.') *> P.digits).maybe
    def number: Parser[Double] =
      P.digits.maybe.flatMap {
        case Some(x) =>
          frac >>= {
            case Some(y) => pure(s"$x.$y".toDouble)
            case None    => pure(x.toDouble)
          }
        case None =>
          frac >>= {
            case Some(y) => pure(s"0.$y".toDouble)
            case None    => fail("expected digits after '.'")
          }
      } label "numeric literal"

    def ident: Parser[String] =
      (cond(_.isLetter, "not a letter") ** cond(
        _.isLetterOrDigit,
        "non-letter/digit"
      ).many map ((a, b) => a :: b)).slice scope "identifier"

    def factor: Parser[Tree] =
      (funcall.attempt |
        (char('(') *> expr <* char(')')).attempt |
        ident.map(Ident(_)) |
        number.map(i => {
          Num(i)
        })).token scope "factor"

    def power: Parser[Tree] =
      factor ** (token("^") *> factor).maybe map {
        case (a, Some(b)) => BinOp("^", a, b)
        case (a, None)    => a
      }

    def unop: Parser[Tree] =
      (charSet("-$").slice.token ** power).map(UnOp(_, _)) | power

    def term: Parser[Tree] =
      unop ** (charSet("*/%").token ** unop).many map { case (a, b) =>
        b.foldLeft(a) { case (a, (op, b)) =>
          BinOp(op.toString, a, b)
        }
      }
    def expr: Parser[Tree] = term ** (charSet("+-").token ** term).many map {
      case (a, b) =>
        b.foldLeft(a) { case (a, (op, b)) =>
          // println(List(a, op, b))
          BinOp(op.toString, a, b)
        }
    } scope "expression"

    def vardef: Parser[Tree] =
      (string("let") *> ident.token <* token("=")) ** expr map {
        case (name, value) => VarDef(name, value)
      } scope "variable definition"

    def varmut: Parser[Tree] =
      (ident.token <* token("=")).token ** expr map { case (name, value) =>
        VarMut(name, value)
      } scope "variable mutation"

    def fundef: Parser[Tree] =
      (token("let").token *> ident.token <* token("(")).token
        ** (ident.token.sep(token(",")).map(_.toList) <* token(")")).token
        ** (token("=") *> expr) map { case ((name, args), body) =>
          FunDef(name, args, body)
        } scope "function definition"
    def funcall: Parser[Tree] =
      (ident.token <* token("(")).token
        ** (expr.sep(token(",")).map(_.toList) <* token(")")).token map {
          case (name, args) => FunCall(name, args)
        } scope "function call"
    def returnStmt: Parser[Tree] =
      (token("return") *> expr) map (Return(_)) scope "return statement"

    def statement: Parser[Tree] =
      (fundef | vardef | varmut | funcall).token.attempt scope "statement"

    def program: Parser[Tree] =
      statement.many ** returnStmt map { case (stmts, ret) =>
        Program(stmts :+ ret)
      }

    program
  end mkParser
end TestParser
