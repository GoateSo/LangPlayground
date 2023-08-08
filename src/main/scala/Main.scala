import util.parsercomb.exparsers.*
import regexmatch.*
import util.parsercomb.*
import util.parsercomb.MyParsers.*

import testparser1.*
import Instruction.*
import Codegen.*

@main def foo =
  val program = """
let x = 1 + 2 * 3
let y = 4 + 5 * 6
let f(x) = x * 2
let g(x,a,b) = x * 3 + y*a
return f(x)
"""
  val tokens = Tokenizer.tokenize(program)
  val tree = Utils.optimize(Parser.parse(tokens))

  val Chunk(inst, consts, syms, upvals, fns, params, _ ) =
    processStmt(tree, Chunk(Nil, Map(), Map(), Map(), Nil, 0))

  println("constants:")
  consts.foreach(println)
  println("symbols:")
  syms.foreach(println)
  println("instructions:")
  inst.foreach(println)

  println("-" * 80)

  fns.foreach { fn =>
    println("function:")
    fn.instructions.foreach(println)
    println("with:")
    println("constants")
    fn.constTable.foreach(println)
    println("symbols")
    fn.symTable.foreach(println)
    println("upvals")
    fn.upvalTable.foreach(println)
    println()
  }
