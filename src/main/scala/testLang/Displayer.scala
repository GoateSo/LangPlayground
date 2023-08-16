package testLang

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer

private def asInstr(inst: Int): Instruction =
  val op = inst & 0x3f
  val a = (inst >> 6) & 0xff
  val b = (inst >> 14) & 0x1ff
  val c = (inst >> 23) & 0x1ff
  val bx = (inst >> 14) & 0x3ffff
  import Instruction.*
  op match
    case 0  => MOVE(a, b)
    case 1  => LOADK(a, bx)
    case 4  => GETUPVAL(a, b)
    case 12 => ADD(a, b, c)
    case 13 => SUB(a, b, c)
    case 14 => MUL(a, b, c)
    case 15 => DIV(a, b, c)
    case 16 => MOD(a, b, c)
    case 17 => POW(a, b, c)
    case 18 => UNM(a, b)
    case 19 => INPUT(a, b)
    case 28 => CALL(a, b)
    case 30 => RETURN(a)
    case 36 => CLOSURE(a, bx)
    case _  => throw Exception("invalid opcode" + op)

object ByteOperations:
  extension (bcStream: ByteArrayInputStream)
    def readInt =
      val b1 = bcStream.read()
      val b2 = bcStream.read()
      val b3 = bcStream.read()
      val b4 = bcStream.read()
      b1 | (b2 << 8) | (b3 << 16) | (b4 << 24)
    def readDouble =
      ByteBuffer.wrap(bcStream.readNBytes(8).reverse).getDouble()
    def readInstr: Instruction = asInstr(bcStream.readInt)
object Displayer {
  import ByteOperations.*

  def decompile(bcStream: ByteArrayInputStream): Unit =
    val nup = bcStream.read()
    val nparam = bcStream.read()
    val nreg = bcStream.read()
    println(s"$nup upvalues, $nparam param, $nreg registers needed")
    val ninst = bcStream.readInt
    println(s"$ninst instructions")
    for _ <- 1 to ninst do println(bcStream.readInstr)
    val nconst = bcStream.readInt
    println(s"$nconst constants")
    for i <- 1 to nconst do println(s"${-i}: ${bcStream.readDouble}")
    val nfn = bcStream.readInt
    println(s"$nfn functions")
    println()
    for _ <- 1 to nfn do decompile(bcStream)
  def disp(bytecode: List[Byte]): Unit = decompile(
    ByteArrayInputStream(bytecode.toArray)
  )
}
