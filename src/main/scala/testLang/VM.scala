package testLang

import scala.collection.mutable.ArrayBuffer
import scala.util.boundary

def getOpCode(instr: Int): Int = instr & 0x3f
def getA(instr: Int): Int = (instr >> 6) & 0xff
def getB(instr: Int): Int = (instr >> 14) & 0x1ff
def getC(instr: Int): Int = (instr >> 23) & 0x1ff

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
val OP_RETURN = 30
val OP_CALL = 28
val OP_CLOSURE = 36

class StackFrame(
    val constants: Array[Double],
    val registers: Array[RegValue],
    val instructions: Array[Int],
    var pc: Int
) {
  override def toString: String =
    s"StackFrame(\n${registers.toList}\n)"
}

class State(
    val frames: ArrayBuffer[StackFrame],
    val protos: Array[Prototype],
    val inputs: Array[Double],
    val closures: ArrayBuffer[Closure] = ArrayBuffer.empty
)

def getOpArg(
    regs: Array[RegValue],
    consts: Array[Double],
    i: Int
): Double =
  if (i > 0xff)
    consts(i & 0xff)
  else
    regs(i) match
      case d: Double => d
      case _ =>
        println(s"regs: ${regs.toList}")
        throw new Exception(s"invalid operand ${i}: ${regs(i)}")

// imperfect but it shall do as a proof of concept
object VM:
  import ByteOperations.*
  def execute(pstate: State): Double =
    import Instruction.*
    var target = pstate.frames.last
    var instrs = target.instructions
    var regs = target.registers
    var consts = target.constants
    var cl = pstate.closures.last
    boundary {
      while target.pc < instrs.length do
        println((target.pc, asInstr(instrs(target.pc))))
        println(s"regs: ${regs.toList}")
        println(s"frames: ${pstate.frames.toList}")
        asInstr(instrs(target.pc)) match
          case LOADK(a, bx) =>
            regs(a) = consts(bx - 256)
          case MOVE(a, b) => regs(a) = regs(b)
          case ADD(a, b, c) =>
            regs(a) = getOpArg(regs, consts, b) + getOpArg(regs, consts, c)
          case SUB(a, b, c) =>
            regs(a) = getOpArg(regs, consts, b) - getOpArg(regs, consts, c)
          case MUL(a, b, c) =>
            regs(a) = getOpArg(regs, consts, b) * getOpArg(regs, consts, c)
          case DIV(a, b, c) =>
            regs(a) = getOpArg(regs, consts, b) / getOpArg(regs, consts, c)
          case MOD(a, b, c) =>
            regs(a) = getOpArg(regs, consts, b) % getOpArg(regs, consts, c)
          case POW(a, b, c) =>
            regs(a) =
              math.pow(getOpArg(regs, consts, b), getOpArg(regs, consts, c))
          case UNM(a, b)   => regs(a) = -getOpArg(regs, consts, b)
          case INPUT(a, b) => regs(a) = pstate.inputs(b)
          case RETURN(a) => // return from function w/ value in register a
            val retVal = regs(a)
            pstate.frames.remove(pstate.frames.length - 1)
            pstate.closures.remove(pstate.closures.length - 1)
            println("returning + " + pstate.frames + " " + retVal)
            if pstate.frames.length > 0 then
              target = pstate.frames.last
              instrs = target.instructions
              regs = target.registers
              consts = target.constants
              cl = pstate.closures.last

              // assert that it corresponds to call
              assert((instrs(target.pc) & 0x3f) == OP_CALL)
              regs(getA(instrs(target.pc))) = retVal
              println("returning to " + target)
            else
              retVal match
                case d: Double => return d
                case _         => throw new Exception("invalid return value")
          case CALL(a, b) => // call function at register a w/ b-1 args
            val fnInd = regs(a) match
              case i: Int    => i
              case d: Double => throw Exception("can't call number")
            val closure = pstate.closures(fnInd)
            // create new stack frame for call
            val nframe = StackFrame(
              closure.proto.consts,
              Array.ofDim(closure.proto.regs),
              closure.proto.chunk,
              0
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
          case CLOSURE(a, bx) => // dest = a, protoInd = bx
            val proto = pstate.protos(bx)
            val ncl = newClosure(proto)
            // println(ncl.upvals.length)
            // println("upvals" + proto.upvalues)
            for i <- 1 to proto.upvalues do
              val pInst = instrs(target.pc + i)
              val ind = getB(pInst)
              println((getOpCode(pInst), ind))
              ncl.upvals(i - 1) = getOpCode(pInst) match
                case OP_MOVE => // in symtable of current frame
                  // println("parent local")
                  (pstate.frames.length - 1, ind)
                case OP_GETUPVAL => // in parent upval (NOT USED)
                  cl.upvals(ind)
                case _ =>
                  throw new Exception(
                    "invalid psuedo instruction " + asInstr(pInst)
                  )
            target.pc += proto.upvalues
            // add to closures list and save index in register
            println(s"new closure upvals: ${ncl.upvals.toList}")
            regs(a) = pstate.closures.length
            pstate.closures.append(ncl)
          case GETUPVAL(a, b) =>
            println(cl.upvals.toList)
            regs(a) = cl.upvals(b) match
              case (frameInd, regInd) =>
                pstate.frames(frameInd).registers(regInd)
              case d: Double => d
              case x         => throw new Exception(s"invalid operand (${x})")
        target.pc += 1
      -1
    }
  def run(p: Prototype, inputs: Array[Double]) =
    execute(
      State( // inital state w/ single frame
        ArrayBuffer(
          StackFrame(
            p.consts,
            Array.ofDim(p.regs),
            p.chunk,
            0
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
