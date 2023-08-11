package testparser1

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer

object Displayer {
  def readInt(bcStream: ByteArrayInputStream): Int =
    val b1 = bcStream.read()
    val b2 = bcStream.read()
    val b3 = bcStream.read()
    val b4 = bcStream.read()
    b1 | (b2 << 8) | (b3 << 16) | (b4 << 24)

  def readInstr(bcStream: ByteArrayInputStream): Unit =
    val inst = readInt(bcStream)
    val op = inst & 0x3f
    val a = (inst >> 6) & 0xff
    val b = (inst >> 14) & 0x1ff
    val c = (inst >> 23) & 0x1ff
    val bx = (inst >> 14) & 0x3ffff
    println(
      op match
        case 0  => s"MOVE $a $b"
        case 1  => s"LOADK $a $bx"
        case 4  => s"GETUPVAL $a $b"
        case 12 => s"ADD $a $b $c"
        case 13 => s"SUB $a $b $c"
        case 14 => s"MUL $a $b $c"
        case 15 => s"DIV $a $b $c"
        case 16 => s"MOD $a $b $c"
        case 17 => s"POW $a $b $c"
        case 18 => s"UNM $a $b"
        case 19 => s"INPUT $a $b"
        case 28 => s"CALL $a $b"
        case 30 => s"RETURN $a $b"
        case 36 => s"CLOSURE $a $bx"
        case _  => "invalid opcode"
    )

  def readDouble(bcStream: ByteArrayInputStream): Double =
    ByteBuffer.wrap(bcStream.readNBytes(8).reverse).getDouble()

  def decompile(bcStream: ByteArrayInputStream): Unit =
    val nup = bcStream.read()
    val nparam = bcStream.read()
    val nreg = bcStream.read()
    println(s"$nup upvalues, $nparam param, $nreg registers needed")
    val ninst = readInt(bcStream)
    println(s"$ninst instructions")
    for _ <- 1 to ninst do readInstr(bcStream)
    val nconst = readInt(bcStream)
    println(s"$nconst constants")
    for i <- 1 to nconst do println(s"${-i}: ${readDouble(bcStream)}")
    val nfn = readInt(bcStream)
    println(s"$nfn functions")
    println()
    for _ <- 1 to nfn do decompile(bcStream)
  def disp(bytecode: List[Byte]): Unit = decompile(
    ByteArrayInputStream(bytecode.toArray)
  )
}
