package testlang1
// make a recursive descent expression parser
/*
grammar:

expr     ::= term (('+' | '-') term)*
term     ::= unop (('*' | '/' | '%') unop)*
unop     ::= '-' unop | power
power    ::= factor ('^' unop)*
factor   ::= number | ident | funcall | '(' expr ')'

funcall  ::= ident '(' (expr (',' expr)*)? ')'
fundef   ::= 'def' ident '(' (ident (',' ident)*)? ')' '=' expr
vardef   ::= 'let' ident '=' expr
varass   ::= ident '=' expr

stmt     ::= fundef | vardef | funcall | varass

program  ::= stmt* 'return' expr

ident    ::= [a-zA-Z_][a-zA-Z0-9_]*
number   ::= '-'? [0-9]+ ('.' [0-9]+)?
 */

enum TreeNode:
  case BinOp(op: String, left: TreeNode, right: TreeNode)
  case UnOp(op: String, right: TreeNode)
  case VarDef(name: String, value: TreeNode)
  case VarMut(name: String, value: TreeNode)
  case FunDef(name: String, args: List[String], body: TreeNode)
  case FunCall(name: String, args: List[TreeNode])
  case Num(value: Double)
  case Ident(name: String)
  case Return(expr: TreeNode)
  case Program(stmts: List[TreeNode])

object Parse {
  import Tokenizer.Token
  import Tokenizer.Token.*

  // parse argument list in function call, expects that opening parn is already consumed
  def parseArgs(cur: List[Token]): (List[TreeNode], List[Token]) =
    val (arg, rest) = expr(cur)
    rest match
      case Op(",") :: next =>
        val (args, rest2) = parseArgs(next); (arg :: args, rest2)
      case Op(")") :: rest => (List(arg), rest)
      case _ =>
        throw new Exception(
          s"expected ',' or ')' in function call, instead got $rest"
        )

  def factor(cur: List[Token]): (TreeNode, List[Token]) = cur match
    case Nil => throw new Exception("unexpected end of input")
    // function call
    case Ident(fname) :: Op("(") :: next =>
      val (args, rest) = parseArgs(next)
      (TreeNode.FunCall(fname, args), rest)
    // variable
    case Ident(name) :: next => (TreeNode.Ident(name), next)
    // number
    case Num(value) :: next => (TreeNode.Num(value), next)
    // parenthesized expression
    case Op("(") :: next =>
      val (expr, rest) = Parse.expr(next)
      rest match
        case Op(")") :: rest2 => (expr, rest2)
        case _ => throw Exception("expected ')' to close '(' in expression")
    case _ => throw Exception(s"unexpected tokens $cur")

  def power(cur: List[Token]): (TreeNode, List[Token]) =
    Parse.factor(cur) match
      case (lhs, Op("^") :: next) =>
        val (rhs, rest2) = Parse.unop(next)
        (TreeNode.BinOp("^", lhs, rhs), rest2)
      case other => other

  def unop(cur: List[Token]): (TreeNode, List[Token]) = cur match
    case Nil => throw new Exception("unexpected end of input")
    case Op(c) :: next if c == "-" || c == "$" =>
      val (rhs, rest) = Parse.unop(next)
      (TreeNode.UnOp(c, rhs), rest)
    case _ => Parse.power(cur)

  def term(cur: List[Token]): (TreeNode, List[Token]) =
    Parse.unop(cur) match
      case (lhs, Op(op) :: next) if op == "*" || op == "/" || op == "%" =>
        val (rhs, rest2) = Parse.term(next)
        (TreeNode.BinOp(op, lhs, rhs), rest2)
      case other => other

  def expr(cur: List[Token]): (TreeNode, List[Token]) =
    Parse.term(cur) match
      case (lhs, Op(op) :: next) if op == "+" || op == "-" =>
        val (rhs, rest2) = Parse.expr(next)
        (TreeNode.BinOp(op, lhs, rhs), rest2)
      case other => other

  def parseParams(cur: List[Token]): (List[String], List[Token]) =
    cur match
      case Ident(n) :: Op(")") :: next => (List(n), next)
      case Ident(n) :: Op(",") :: next =>
        val (args, rest2) = parseParams(next)
        (n :: args, rest2)
      case _ =>
        throw Exception("expected ')' to close '(' in function call")

  def parseStmt(cur: List[Token]): (TreeNode, List[Token]) = cur match
    case Ident("let") :: Ident(n) :: next =>
      next match
        // vardef
        case Op("=") :: rest =>
          val (exp, rem) = expr(rest)
          (TreeNode.VarDef(n, exp), rem)
        // fundef
        case Op("(") :: rest =>
          val (params, rem) = parseParams(rest)
          if (rem.head != Op("=")) then
            throw Exception(
              "bad definition: expected '=' after function params"
            )
          val (exp, rem2) = expr(rem.tail)
          // println(s"expression: $exp remainder: $rem2")
          (TreeNode.FunDef(n, params, exp), rem2)
        case _ =>
          throw Exception(
            "bad definition: expected '=' or '(' after 'let <name>'"
          )
    // function call
    case Ident(n) :: Op("(") :: next =>
      val (args, rest) = parseArgs(next)
      (TreeNode.FunCall(n, args), rest)
    case Ident(n) :: Op("=") :: next =>
      println("varmut")
      val (exp, rest) = expr(next)
      (TreeNode.VarMut(n, exp), rest)
    case _ => throw Exception(s"unexpected tokens $cur")

  def parseStmts(cur: List[Token]): (List[TreeNode], List[Token]) =
    cur match
      case Ident("return") :: next =>
        val (exp, rest) = expr(next)
        (List(TreeNode.Return(exp)), rest)
      case _ =>
        if cur.isEmpty then (Nil, cur)
        else // perhaps add statement check??
          val (stmt, rest) = parseStmt(cur)
          val (stmts, rest2) = parseStmts(rest)
          (stmt :: stmts, rest2)

  // expr
  def parse(tokens: List[Token]): TreeNode =
    val (tree, rest) = parseStmts(tokens)
    if rest.isEmpty then TreeNode.Program(tree)
    else throw new Exception("unexpected tokens after end of program")

  def apply(tokens: List[Token]): TreeNode = parse(tokens)
}
