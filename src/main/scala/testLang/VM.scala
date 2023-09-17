package testLang

import scala.collection.mutable.ArrayBuffer
import java.io.ByteArrayInputStream
import Func.*

object PState:
  class StackFrame(
      val constants: Array[Double],
      val registers: Array[RegValue],
      val instructions: Array[Int],
      var pc: Int,
      val closure: Closure
  )

  class State(
      val frames: ArrayBuffer[StackFrame],
      val protos: Array[Prototype],
      val inputs: Array[Double],
      val closures: ArrayBuffer[Closure] = ArrayBuffer.empty,
      var retVal: Double = -1
  )

object OpCode:
  val OP_MOVE = 0
  val OP_LOADK = 1
  val OP_GETUPVAL = 4
  val OP_ADD = 12
  val OP_SUB = 13
  val OP_MUL = 14
  val OP_DIV = 15
  val OP_MOD = 16
  val OP_POW = 17
  val OP_UNM = 18
  val OP_INPUT = 19
  val OP_CALL = 28
  val OP_RETURN = 30
  val OP_CLOSURE = 36

  inline def getOpCode(instr: Int): Int = instr & 0x3f
  inline def getA(instr: Int): Int = (instr >> 6) & 0xff
  inline def getB(instr: Int): Int = (instr >> 14) & 0x1ff
  inline def getC(instr: Int): Int = (instr >> 23) & 0x1ff
  inline def getBx(instr: Int): Int = (instr >> 14) & 0x3ffff

// imperfect but it shall do as a proof of concept
object VM:
  import ByteOperations.*
  import PState.*
  import OpCode.*

  private inline def getOpArg(
      i: Int
  )(using frame: StackFrame): Double =
    if i > 0xff then frame.constants(i & 0xff)
    else
      frame.registers(i) match
        case d: Double => d
        case _ =>
          throw new Exception(s"invalid operand ${i}: ${frame.registers(i)}")
  def execute(pstate: State): Double =
    import Instruction.*
    implicit var target: StackFrame = pstate.frames.last
    var instrs = target.instructions
    var regs = target.registers
    var consts = target.constants
    var cl = pstate.closures.last
    var hasRet = false
    while target.pc < instrs.length do
      val a = getA(instrs(target.pc))
      val b = getB(instrs(target.pc))
      val c = getC(instrs(target.pc))
      val bx = getBx(instrs(target.pc))
      getOpCode(instrs(target.pc)) match
        case OP_MOVE =>
          regs(a) = regs(b)
        case OP_LOADK =>
          regs(a) = consts(bx - 256)
        case OP_GETUPVAL =>
          regs(a) = cl.upvals(b) match
            case (frameInd, regInd) =>
              pstate.frames(frameInd).registers(regInd)
            case d: Double => d
            case x         => throw new Exception(s"invalid operand (${x})")
        case OP_ADD =>
          regs(a) = getOpArg(b) + getOpArg(c)
        case OP_SUB =>
          regs(a) = getOpArg(b) - getOpArg(c)
        case OP_MUL =>
          regs(a) = getOpArg(b) * getOpArg(c)
        case OP_DIV =>
          regs(a) = getOpArg(b) / getOpArg(c)
        case OP_MOD =>
          regs(a) = getOpArg(b) % getOpArg(c)
        case OP_POW =>
          regs(a) = math.pow(getOpArg(b), getOpArg(c))
        case OP_UNM =>
          regs(a) = -getOpArg(b)
        case OP_INPUT =>
          regs(a) = pstate.inputs(b - 256)
        case OP_CALL =>
          val fnInd = regs(a) match
            case i: Int    => i
            case d: Double => throw Exception("can't call number")
          val closure = pstate.closures(fnInd)
          // create new stack frame for call
          val nframe = StackFrame(
            closure.proto.consts,
            Array.ofDim(closure.proto.regs),
            closure.proto.chunk,
            0,
            closure // save closure for return
          )
          // initialize parameter registers
          for i <- 0 until b - 1 do nframe.registers(i) = regs(a + i + 1)
          // update current frame ptr
          target = nframe
          target.pc -= 1

          instrs = target.instructions
          regs = target.registers
          consts = target.constants
          cl = closure
          // append frame to stack
          pstate.frames.append(nframe)
        case OP_RETURN =>
          val retVal = regs(a)
          pstate.frames.remove(pstate.frames.length - 1)
          if pstate.frames.length > 0 then
            cl = target.closure
            target = pstate.frames.last
            instrs = target.instructions
            regs = target.registers
            consts = target.constants
            // assert that it corresponds to call
            assert(
              (instrs(target.pc) & 0x3f) == OP_CALL,
              "return must correspond to call"
            )
            regs(getA(instrs(target.pc))) = retVal
          else
            hasRet = true
            pstate.retVal = retVal match
              case d: Double => return d
              case _         => throw new Exception("invalid return value")
        case OP_CLOSURE =>
          val proto = pstate.protos(bx)
          val ncl = newClosure(proto)
          for i <- 1 to proto.upvalues do
            val pInst = instrs(target.pc + i)
            val ind = getB(pInst)
            ncl.upvals(i - 1) = getOpCode(pInst) match
              case OP_MOVE => // in symtable of current frame
                (pstate.frames.length - 1, ind)
              case OP_GETUPVAL => // in parent upval (NOT USED)
                cl.upvals(ind)
              case _ => throw Exception("invalid psuedo inst")
          target.pc += proto.upvalues
          // add to closures list and save index in register
          regs(a) = pstate.closures.length
          pstate.closures.append(ncl)
      target.pc += 1
    assert(hasRet, "program must have return value")
    pstate.retVal

  def run(bytes: Array[Byte], inputs: Array[Double]) =
    val p = Reader.parse(ByteArrayInputStream(bytes))
    execute(
      State( // inital state w/ single frame
        ArrayBuffer(
          StackFrame(
            p.consts,
            Array.ofDim(p.regs),
            p.chunk,
            0,
            null // top level has no previous closure
          )
        ),
        p.protos,
        inputs,
        ArrayBuffer(
          Closure(p, Array.empty)
        )
      )
    )
end VM
