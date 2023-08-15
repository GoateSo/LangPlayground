package testLang

import scala.io.Source
import java.io.ByteArrayInputStream

object Utils:
  import TreeNode.*

  enum InputType:
    case FileName, Prog

  def run(
      itype: InputType,
      input: String,
      numInputs: List[Double] = List.empty
  ): Unit =
    val inputStr = itype match
      case InputType.FileName => Source.fromFile(input).mkString("")
      case InputType.Prog     => input
    val tokens = Tokenizer.tokenize(inputStr)
    val ast = Parser.parse(tokens)
    println(s"""result: ${eval(
        ast,
        Map(
          "PI" -> Num(Math.PI),
          "E" -> Num(Math.E)
        ),
        numInputs
      )._1}""")

  private val BinopMap = Map[String, (Double, Double) => Double](
    "+" -> (_ + _),
    "-" -> (_ - _),
    "*" -> (_ * _),
    "/" -> (_ / _),
    "%" -> (_ % _),
    "^" -> math.pow
  )
  def optimize(node: TreeNode): TreeNode = node match
    case BinOp(op, left, right) =>
      val l = optimize(left)
      val r = optimize(right)
      (l, r) match
        case (Num(lv), Num(rv)) => Num(BinopMap(op)(lv, rv))
        case _                  => BinOp(op, l, r)
    case UnOp(op, right) =>
      optimize(right) match
        case Num(rv) if op == "-" => Num(-rv)
        case _                    => UnOp(op, right)
    case VarDef(name, value)      => VarDef(name, optimize(value))
    case VarMut(name, value)      => VarMut(name, optimize(value))
    case FunDef(name, args, body) => FunDef(name, args, optimize(body))
    case FunCall(name, args)      => FunCall(name, args.map(optimize))
    case Num(value)               => Num(value)
    case Ident(name)              => Ident(name)
    case Return(expr)             => Return(optimize(expr))
    case Program(stmts)           => Program(stmts.map(optimize))

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
      case Num(value)          => value.toString
      case Ident(name)         => name
      case Return(value)       => s"return ${convert(value)}"
      case Program(stmts)      => stmts.map(convert).mkString("\n")
      case VarMut(name, value) => s"$name = ${convert(value)}"

  def dispAST(node: TreeNode, depth: Int = 0): Unit =
    print(s"${"| " * depth}")
    node match
      case BinOp(op, left, right) =>
        println(s"BinOp($op)")
        dispAST(left, depth + 1)
        dispAST(right, depth + 1)
      case UnOp(op, right) =>
        println(s"UnOp($op)")
        dispAST(right, depth + 1)
      case VarDef(name, value) =>
        println(s"VarDef($name)")
        dispAST(value, depth + 1)
      case FunDef(name, args, body) =>
        println(s"FunDef($name)")
        println(s" args: ${args.mkString(", ")}")
        dispAST(body, depth + 1)
      case FunCall(name, args) =>
        println(s"FunCall($name)")
        args.foreach { arg =>
          dispAST(arg, depth + 1)
        }
      case Num(value) =>
        println(s"Num($value)")
      case Ident(name) =>
        println(s"Ident($name)")
      case Return(value) =>
        println(s"Return")
        dispAST(value, depth + 1)
      case Program(stmts) =>
        println(s"Program")
        stmts.foreach { stmt =>
          dispAST(stmt, depth + 1)
        }
      case VarMut(name, value) =>
        println(s"VarMut($name)")
        dispAST(value, depth + 1)

  def eval(
      node: TreeNode,
      env: Map[String, TreeNode],
      inputs: List[Double]
  ): (Double, Map[String, TreeNode]) =
    import TreeNode.*
    node match
      case node @ VarDef(name, value) =>
        (-1, env + (name -> Num(eval(value, env, inputs)._1)))
      case node @ FunDef(name, args, body) => (-1, env + (name -> node))
      case VarMut(name, value) =>
        env.get(name) match
          case Some(x) =>
            (
              -1,
              env + (name -> Num(eval(value, env, inputs)._1))
            ) // override value
          case None =>
            throw Exception(s"cannot assign to unknown variable $name")
      case Num(value)   => (value, env)
      case Return(expr) => eval(expr, env, inputs)
      case FunCall(name, args) =>
        env.get(name) match
          // run function adding everything to the environment, remove everything after
          case Some(FunDef(_, params, body)) =>
            var xs =
              env ++ params.zip(args.map(arg => Num(eval(arg, env, inputs)._1)))
            (eval(body, xs, inputs)._1, env)
          case None =>
            name match
              case "print" =>
                val values = args.map(eval(_, env, inputs)._1)
                println(values mkString "\t");
                (0, env)
              case _ => throw new Exception(s"unknown function $name")
          case _ =>
            throw Exception(
              s"non-function $name cannot be used as function in expression"
            )
      case Ident(name) =>
        env.get(name) match
          case Some(x) => eval(x, env, inputs)
          case _       => throw Exception(s"unknown variable $name")
      case BinOp(op, left, right) =>
        val l = eval(left, env, inputs)._1
        val r = eval(right, env, inputs)._1
        (BinopMap(op)(l, r), env)
      case UnOp(op, right) =>
        val (operand, _) = eval(right, env, inputs)
        op match
          case "-" => (-operand, env)
          case "$" if operand.isValidInt =>
            (inputs(operand.toInt), env)
          case _ =>
            throw Exception(
              s"unknown unary operator $op for given operand ${operand}"
            )
      case Program(stmts) =>
        stmts.foldLeft(0.0, env) { (acc, stmt) =>
          eval(stmt, acc._2, inputs)
        }
  // differing compilation and interpretation option
  def compile(s: String, fold: Boolean = true): Array[Byte] =
    val tree = Parser.parse(Tokenizer.tokenize(s))
    BytecodeWriter
      .toByteStream(
        Codegen.generate(
          if fold then Utils.optimize(tree) else tree
        )
      )
      .toArray
  def compile(s: String, fileName: String): Unit =
    val tree = Parser.parse(Tokenizer.tokenize(s))
    BytecodeWriter.writeToFile(
      Codegen.generate(Utils.optimize(tree)),
      fileName
    )
  def interpret(s: String, inputs: Array[Double]): Double =
    VM.run(compile(s, true), inputs)

  def interpret(xs: Array[Byte], inputs: Array[Double]): Double =
    VM.run(xs, inputs)

end Utils
