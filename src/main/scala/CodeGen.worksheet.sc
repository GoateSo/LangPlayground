import testLang.Reader
import scala.collection.mutable.ArrayBuffer
import testLang.*
import java.io.ByteArrayInputStream
import java.nio.ByteBuffer
import TreeNode.*
import Instruction.*

import Codegen.*

val toChunk =
  Tokenizer.tokenize andThen Parser.parse andThen Codegen.generate

val prog = "let a = 1 let b = 2 let f(x) = x + b + a return f(1)"
val chunk = toChunk(prog)
chunk.fnTable.head.instructions.foreach(println)
