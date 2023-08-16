import testLang.Reader
import scala.collection.mutable.ArrayBuffer
import testLang.*
import java.io.ByteArrayInputStream
import java.nio.ByteBuffer
import TreeNode.*
import Instruction.*

import Codegen.*

val program = """
let x = 1 + 2 * 3
let y = 4 + 5 * 6
let f(x) = x * 2
let g(x,a,b) = x * 3 + y*a - b
let a = f(5)
return g(g(1,1,1),f(2),g(1,f(1),1))
"""

val tokens = Tokenizer.tokenize(program)
val tree = (Parser.parse(tokens))
val st =
  processStmt(tree, Chunk(Nil, Map.empty, Map.empty, Map.empty, Nil, 0))

st.fnTable.head.upvalTable
st.fnTable.head.instructions.foreach(println)

st
BytecodeWriter.toByteStream(st).toList

val strm = ByteArrayInputStream(BytecodeWriter.toByteStream(st).toArray)
Displayer.decompile(strm)

// BytecodeWriter.writeToFile(st)

val xs = BytecodeWriter.toByteStream(st).toArray

VM.run(xs, Array.emptyDoubleArray)
