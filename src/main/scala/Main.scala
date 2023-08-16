import testLang.{Chunk, Codegen, Tokenizer, Utils, Parser}
import util.parsercomb.exparsers.*
import regexmatch.*
import util.parsercomb.*
import util.parsercomb.MyParsers.*

import Codegen.*
import testLang.BytecodeWriter
import testLang.Displayer

@main def foo =
  val program = """
let x = 1 + 2 * 3
let y = 4 + 5 * 6
let f(x) = x * 2
let g(x,a,b) = x * 3 + y*a
return f(x) + f(y)
"""
  val code = Utils.compile(program)
  val res = Utils.interpret(code, Array.empty[Double])
  println(s"result: $res")
