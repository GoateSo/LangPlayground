package testLang
import munit.FunSuite
import Chunk.*
import Instruction.*

private def toChunk =
  Tokenizer.tokenize andThen Parser.parse andThen Codegen.generate

class CodeGenSuite extends FunSuite:
  test("simple program") {
    val code = toChunk("return 1")
    assertEquals(code.instructions, List(LOADK(0, 256), RETURN(0)))
    assertEquals(code.constTable, Map(1.0 -> 256))
    assert(code.fnTable.isEmpty)
  }

  test("variable definitions") {
    val code = toChunk("let a = 1 let b = 2 return a + b")
    assertEquals(code.constTable, Map(1.0 -> 256, 2.0 -> 257))
    assertEquals(
      code.instructions,
      List(
        LOADK(0, 256),
        LOADK(1, 257),
        ADD(2, 0, 1),
        RETURN(2)
      )
    )
    assert(code.fnTable.isEmpty)
  }

  test("variable mutation") {
    val code = toChunk("let a = 1 a = 2 return a")
    assertEquals(
      code.instructions,
      List(
        LOADK(0, 256),
        LOADK(0, 257),
        RETURN(0)
      )
    )
  }
  test("function definition") {
    val code = toChunk("let f(x) = x + 2 return f(1)")
    assertEquals(code.constTable, Map(1.0 -> 256))
    assertEquals(
      code.instructions,
      List(
        CLOSURE(0, 0),
        MOVE(1, 0),
        LOADK(2, 256),
        CALL(1, 2),
        RETURN(1)
      )
    )
  }

  test("closure with upvalues") {
    val code = toChunk("let a = 1 let b = 2 let f(x) = x + b + a return f(1)")
    assertEquals(code.constTable, Map(1.0 -> 256, 2.0 -> 257))
    assertEquals(
      code.instructions,
      List(
        LOADK(0, 256),
        LOADK(1, 257),
        CLOSURE(2, 0),
        MOVE(0, 1),
        MOVE(0, 0),
        MOVE(3, 2),
        LOADK(4, 256),
        CALL(3, 2),
        RETURN(3)
      )
    )
    assertEquals(
      code.fnTable.head.instructions,
      List(
        GETUPVAL(1, 0),
        ADD(1, 0, 1),
        GETUPVAL(2, 1),
        ADD(1, 1, 2),
        RETURN(1)
      )
    )
  }
