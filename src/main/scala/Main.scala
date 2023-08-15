import testLang.{Chunk, Codegen, Tokenizer, Utils, Parser}
import util.parsercomb.exparsers.*
import regexmatch.*
import util.parsercomb.*
import util.parsercomb.MyParsers.*

import testparser1.*
import Codegen.*
import testLang.BytecodeWriter
import testLang.Displayer

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

  val stack =
    processStmt(tree, Chunk(Nil, Map(), Map(), Map(), Nil, 0))

  val bytecode = BytecodeWriter.toByteStream(stack).toList
  Displayer.disp(bytecode)
