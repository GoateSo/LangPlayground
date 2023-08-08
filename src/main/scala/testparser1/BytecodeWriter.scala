package testparser1

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
  import Instruction.*
  import Codegen.*
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
  def toByteStream(program: Chunk, line: Int = 0): List[Byte] =
    val Chunk(
      instructions,
      constTable,
      symTable,
      upvalTable,
      fnTable,
      paramCnt,
      _
    ) = program
    val lastLine = line + instructions.size
    val upvalCount = upvalTable.size.toByte
    val paramCount = paramCnt.toByte
    ???
end BytecodeWriter
