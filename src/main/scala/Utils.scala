object Utils {
  import TreeNode.*
  def convert(node: TreeNode): String =
  node match
    case BinOp(op, left, right) =>
      s"(${convert(left)} $op ${convert(right)})"
    case UnOp(op, right)     => s"($op ${convert(right)})"
    case VarDef(name, value) => s"let $name = ${convert(value)}"
    case FunDef(name, args, body) =>
      s"let $name(${args.mkString(", ")}) = ${convert(body)}"
    case FunCall(name, args) =>
      s"$name(${args.map(convert).mkString(", ")})"
    case Num(value)     => value.toString
    case Ident(name)    => name
    case Return(value)  => s"return ${convert(value)}"
    case Program(stmts) => stmts.map(convert).mkString("\n")

  def dispAST(node: TreeNode, depth: Int = 0): Unit = node match
    case BinOp(op, left, right) =>
      println(s"${"| " * depth}BinOp($op)")
      dispAST(left, depth + 1)
      dispAST(right, depth + 1)
    case UnOp(op, right) =>
      println(s"${"| " * depth}UnOp($op)")
      dispAST(right, depth + 1)
    case VarDef(name, value) =>
      println(s"${"| " * depth}VarDef($name)")
      dispAST(value, depth + 1)
    case FunDef(name, args, body) =>
      println(s"${"| " * depth}FunDef($name)")
      println(s"${"| " * depth} args: ${args.mkString(", ")}")
      dispAST(body, depth + 1)
    case FunCall(name, args) =>
      println(s"${"| " * depth}FunCall($name)")
      args.foreach { arg =>
        dispAST(arg, depth + 1)
      }
    case Num(value) =>
      println(s"${"| " * depth}Num($value)")
    case Ident(name) =>
      println(s"${"| " * depth}Ident($name)")
    case Return(value) =>
      println(s"${"| " * depth}Return")
      dispAST(value, depth + 1)
    case Program(stmts) =>
      println(s"${"| " * depth}Program")
      stmts.foreach { stmt =>
        dispAST(stmt, depth + 1)
      }
}
