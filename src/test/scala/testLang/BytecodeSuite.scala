package testLang

import java.io.ByteArrayInputStream
import testLang.ByteOperations
import testLang.ByteOperations.readInt
import OpCode.*
import testLang.BytecodeSuite.assertInstr
import testLang.ByteOperations.readDouble

object BytecodeSuite extends munit.FunSuite:
  extension (s: ByteArrayInputStream)
    def assertInstr(op: Int, a: Int, b: Int, c: Int): Unit = {
      val instr = s.readInt
      assertEquals(getOpCode(instr), op, s"bad opcode")
      assertEquals(getA(instr), a, "bad operand A")
      assertEquals(getB(instr), b, "bad operand B")
      assertEquals(getC(instr), c, "bad operand C")
    }
    def assertInstr(op: Int, a: Int, bx: Int): Unit = {
      val instr = s.readInt
      assertEquals(getOpCode(instr), op, "bad opcode")
      assertEquals(getA(instr), a, "bad operand A")
      assertEquals(getBx(instr), bx, "bad operand Bx")
    }
end BytecodeSuite

class BytecodeSuite extends munit.FunSuite:
  test("single const return") {
    val code = toChunk("return 1")
    println(code)
    val strm = ByteArrayInputStream(BytecodeWriter.toByteStream(code).toArray)
    // no upvals/params, only 1 register needed
    assertEquals(strm.read(), 0)
    assertEquals(strm.read(), 0)
    assertEquals(strm.read(), 1)
    // should only be loadk and return
    assertEquals(strm.readInt, 2)
    strm.assertInstr(OP_LOADK, 0, 256)
    strm.assertInstr(OP_RETURN, 0, 0, 0)
    // should have 1 constant
    assertEquals(strm.readInt, 1)
    assertEquals(strm.readDouble, 1.0)
    // should have no functions
    assertEquals(strm.readInt, 0)
    assert(strm.available() == 0)
  }
  test("function def and call") {
    val code = toChunk("let f(x) = x + 1 return f(2)")
    val strm = ByteArrayInputStream(BytecodeWriter.toByteStream(code).toArray)
    // no upvals/params, only 3 registers needed (f, f and 2 during call)
    assertEquals(strm.read(), 0)
    assertEquals(strm.read(), 0)
    assertEquals(strm.read(), 3)
    // should be closure, move, loadk, call, return
    assertEquals(strm.readInt, 5)
    strm.assertInstr(OP_CLOSURE, 0, 0)
    strm.assertInstr(OP_MOVE, 1, 0, 0)
    strm.assertInstr(OP_LOADK, 2, 256)
    strm.assertInstr(OP_CALL, 1, 2, 0)
    strm.assertInstr(OP_RETURN, 1, 0, 0)
    // should have 1 const
    assertEquals(strm.readInt, 1)
    assertEquals(strm.readDouble, 2.0)
    // should have 1 function
    assertEquals(strm.readInt, 1)
    // no upval, 1 param, 2 registers
    assertEquals(strm.read(), 0)
    assertEquals(strm.read(), 1)
    assertEquals(strm.read(), 2)
    // should be add, return
    assertEquals(strm.readInt, 2)
    strm.assertInstr(OP_ADD, 1, 0, 256)
    strm.assertInstr(OP_RETURN, 1, 0, 0)
  }
  test("function def with upvalues") {
    val code = toChunk("let a = 1 let f(x) = x + a return f(2)")
    val strm = ByteArrayInputStream(BytecodeWriter.toByteStream(code).toArray)
    // no upval/ param, 4 registers (a, f, and two for f and 2 during call)
    assertEquals(strm.read(), 0)
    assertEquals(strm.read(), 0)
    assertEquals(strm.read(), 4)
    // should be loadk, closure, move, loadk, call, return
    // plus one for the closure psuedo instruction for upvalue
    assertEquals(strm.readInt, 7)
    strm.assertInstr(OP_LOADK, 0, 256)
    strm.assertInstr(OP_CLOSURE, 1, 0)
    strm.assertInstr(OP_MOVE, 0, 0, 0)
    strm.assertInstr(OP_MOVE, 2, 1, 0)
    strm.assertInstr(OP_LOADK, 3, 257)
    strm.assertInstr(OP_CALL, 2, 2, 0)
    strm.assertInstr(OP_RETURN, 2, 0, 0)
    // should have 2 const
    assertEquals(strm.readInt, 2)
    assertEquals(strm.readDouble, 1.0)
    assertEquals(strm.readDouble, 2.0)
    // should have 1 function
    assertEquals(strm.readInt, 1)
    // 1 upval, 1 param, 2 registers
    assertEquals(strm.read(), 1)
    assertEquals(strm.read(), 1)
    assertEquals(strm.read(), 2)
    // should be getupval, add, return
    assertEquals(strm.readInt, 3)
    strm.assertInstr(OP_GETUPVAL, 1, 0, 0)
    strm.assertInstr(OP_ADD, 1, 0, 1)
    strm.assertInstr(OP_RETURN, 1, 0, 0)
  }
