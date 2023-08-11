import java.io.ByteArrayInputStream
import java.nio.ByteBuffer
import testparser1.*
import TreeNode.*

import Instruction.*
import Codegen.*

val program = """
let x = 1
let f(a) = a * x
return f(x)
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
