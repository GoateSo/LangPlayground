package testLang

import java.nio.ByteBuffer
import Instruction.*
import Codegen.*
import scala.collection.immutable.Queue
import java.io.File
import java.io.FileOutputStream
/*
-- Notations:

R: registers
C: constants
RC: either regist or constant

A : register A
Ax: register A extended

iABC format:
  Op: bits 0-5
  A: bits 6-13
  B: bits 14-22
  C: bits 23-31
iABx format:
  Op: bits 0-5
  A: bits 6-13
  Bx: bits 14-31

extended signed register (sBx) unused b/c JMP and FORLOOP instructions aren't present

operations: args : description
  LOADK: A, Bx         : R[A] := C[Bx] // Bx is an index into the constant table
  MOVE: A, B       : R[A] := R[B]
  ADD: A, B, C     : R[A] := RC[B] + RC[B]
  SUB: A, B, C     : R[A] := RC[B] - RC[B]
  MUL: A, B, C     : R[A] := RC[B] * RC[B]
  DIV: A, B, C     : R[A] := RC[B] / RC[B]
  MOD: A, B, C     : R[A] := RC[B] % RC[B]
  POW: A, B, C     : R[A] := RC[B] ^ RC[B]
  UNM: A, B        : R[A] := -RC[B]
  INPUT: A, B      : R[A] := inputs[B] // inputs table to be passed in at runtimes
  // note: functions here can only return 1 value, so additional register not neede
  RETURN: A        : return R[A]
  CALL: A, B       : R[A] := RC[A](RC[A+1]..RC[A+B-1])
  CLOSURE: A, Bx   : R[A] := closure(KPROTO[Bx], R[A]..R[A+n])
  GETUPVAL: A, B   : R[A] := Upval[B]
 */

/*
opcodes:
  MOVE=0
  LOADK=1
  GETUPVAL=4
  ADD=12
  SUB=13
  MUL=14
  DIV=15
  MOD=16
  POW=17
  UNM=18
  INPUT=19 // only val outside lua vm spec
  CALL=28
  RETURN=30
  CLOSURE=36
 */
object BytecodeWriter:
  // each instruction corresponds to 4 byte segment in bytecode

  // iABC format
  // CCCCCCCCCBBBBBBBBBAAAAAAAAOOOOOO
  // 9 bit- 9 bit- 8 bit- 6 bit
  private inline def toInst(inline op: Int, a: Int, b: Int, c: Int): Int =
    (op & 0x3f) | ((a & 0xff) << 6) | ((b & 0x1ff) << 14) | ((c & 0x1ff) << 23)
  // iABx format
  // BBBBBBBBBBBBBBBBBAAAAAAAAOOOOOO
  // 18 bit- 8 bit- 6 bit
  private inline def toInst(inline op: Int, a: Int, bx: Int): Int =
    (op & 0x3f) | ((a & 0xff) << 6) | ((bx & 0x3ffff) << 14)

  private inline def toOpCode(ist: Instruction): Int = ist match
    case ADD(a, b, c)   => toInst(12, a, b, c)
    case SUB(a, b, c)   => toInst(13, a, b, c)
    case MUL(a, b, c)   => toInst(14, a, b, c)
    case DIV(a, b, c)   => toInst(15, a, b, c)
    case MOD(a, b, c)   => toInst(16, a, b, c)
    case POW(a, b, c)   => toInst(17, a, b, c)
    case UNM(a, b)      => toInst(18, a, b, 0)
    case INPUT(a, b)    => toInst(19, a, b, 0)
    case RETURN(a)      => toInst(30, a, 0, 0)
    case CALL(a, b)     => toInst(28, a, b, 0)
    case CLOSURE(a, bx) => toInst(36, a, bx)
    case GETUPVAL(a, b) => toInst(4, a, b, 0)
    case LOADK(a, bx)   => toInst(1, a, bx)
    case MOVE(a, b)     => toInst(0, a, b, 0)

  private inline def constBytes(c: Double): Array[Byte] =
    ByteBuffer
      .allocate(8)
      .putDouble(c)
      .array()
      .reverse // maybe not needed?

  extension (buf: Queue[Byte])
    private def writeByte(b: Byte): Queue[Byte] = buf.enqueue(b)
    // little write int:
    private def writeInt(i: Int): Queue[Byte] =
      buf.enqueueAll(
        List(
          (i & 0xff).toByte,
          ((i >> 8) & 0xff).toByte,
          ((i >> 16) & 0xff).toByte,
          ((i >> 24) & 0xff).toByte
        ) // .reverse?
      )
    private def writeInstrs(xs: List[Instruction]): Queue[Byte] =
      xs.foldLeft(buf)((buf, x) => buf.writeInt(toOpCode(x)))

    private def writeConsts(xs: List[Double]): Queue[Byte] =
      xs.foldLeft(buf)((buf, x) => buf.enqueueAll(constBytes(x)))

  private def getMaxRegister(c: Chunk): Int =
    c.instructions.foldLeft(0)((acc, x) =>
      x match
        case MOVE(a, b)     => Math.max(a, acc)
        case LOADK(a, _)    => Math.max(a, acc)
        case GETUPVAL(a, _) => Math.max(a, acc)
        case ADD(a, b, c)   => Math.max(a, acc)
        case SUB(a, b, c)   => Math.max(a, acc)
        case MUL(a, b, c)   => Math.max(a, acc)
        case DIV(a, b, c)   => Math.max(a, acc)
        case MOD(a, b, c)   => Math.max(a, acc)
        case POW(a, b, c)   => Math.max(a, acc)
        case UNM(a, b)      => Math.max(a, acc)
        case INPUT(a, b)    => Math.max(a, acc)
        case RETURN(a)      => Math.max(a, acc)
        case CALL(a, b)     => Math.max(a, acc)
        case CLOSURE(a, _)  => Math.max(a, acc)
    )
  // Chunk as top level function block
  // Int: line defined
  // Int: last line defined
  // 1 byte: number of upvalues
  // 1 byte: number of parameters
  // 1 byte: stack size (# registers used)
  // List of instructions
  // List of constants
  // List of functions
  // List of locals
  // List of upvalues
  // TODO: record upvalues with level and register index
  def toByteStream(program: Chunk): Queue[Byte] =
    val Chunk(
      instructions,
      constTable,
      symTable,
      upvalTable,
      fnTable,
      paramCnt,
      _
    ) = program
    // val lastLine = line + instructions.size
    println(constTable.toList.sortBy(_._2).map(_._1))
    val list = Queue
      .empty[Byte] // preamble
      // .writeInt(line)
      // .writeInt(lastLine)
      .writeByte(upvalTable.size.toByte)
      .writeByte(paramCnt.toByte)
      // TODO: write # of registers used
      .writeByte((getMaxRegister(program) + 1).toByte)
      // instructions
      .writeInt(instructions.size)
      .writeInstrs(instructions)
      // constants
      .writeInt(constTable.size)
      .writeConsts(
        constTable.toList.sortBy(_._2).map(_._1)
      )
      // functions
      .writeInt(fnTable.size)
    // write function impls
    fnTable.foldLeft(list)((buf, fn) =>
      val dumped = toByteStream(fn)
      buf.enqueueAll(dumped)
    )

  def writeToFile(program: Chunk, fileName: String = "bytecode.out") =
    val bytes = toByteStream(program)
    val file = File(fileName)
    val output = FileOutputStream(file)
    output.write(bytes.toArray)

end BytecodeWriter
