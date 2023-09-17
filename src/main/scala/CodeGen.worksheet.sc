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

val prog = "return 1+2/3-4*1^2*-1"
val tree = Parser.parse(Tokenizer.tokenize(prog))
Utils.eval(tree, Map.empty, List.empty)
Utils.dispAST(tree)
val chunk = toChunk(prog)
