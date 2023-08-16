package testLang
import TreeNode.*
import Instruction.*

// language features and omissions:
/*
** -> no loop instructions (FORLOOP, FORPREP, TFORLOOP)
** -> no vararg instructions (VARARG)
** -> no jump instructions (JMP)
** -> no function nesting necessary (but CLOSURE instruction still needed for creation)
** -> no scoping (outside of function params)(CLOSE)
** -> no boolean/conditional instructions (EQ, LT, LE)
** -> no table instructions (GETTABLE, SETTABLE, NEWTABLE)
** -> no string instructions (CONCAT)
** -> no bitwise instructions (SHL, SHR, BAND, BOR, BXOR, BNOT)
 */
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
  LOADK: A, Bx     : R[A] := C[Bx] // Bx is an index into the constant table
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
ideas for implementation:
==============
==statements==
==============
Vardef:
-> process RHS as expr
- always create a new register for the Vardef
- add new varable to symbol table
-> if RHS is a constant, use LOADK instruction
-> if RHS is identifier, use MOVE instruction
-> otherwise, use appropriate arithmetic instructions (helper fn)
Varmut:
- same as Vardef, but don't create a new register
-> otherwise use appropriate arithmetic instruction (helper fn)
Fundef: (function definition)
-> create new function prototype, and add to list of function prototypes
  Translation instructions:
    -create new symbol + constant table for function (empty)
    -create table for upvalues
    -add all parameters to symbol table
    translating function body:
      - translate each statement to instruction
      - if a variable is used that is not in the symbol table, add it to the upvalue table
      - if a constant is used that is not in the constant table, add it to the constant table
- create new register for function, and add to symbol table (b/c its a closure w/ variable)
- store function prototype with assorted information in a function table
- add function to symbol table
- translate to CLOSURE instruction
Return:
- process RHS as expr and translate to RETURN instruction
===============
==expressions==
===============
Binop:
- translate LHS and RHS to instructions
  - if LHS/RHS are constants, use const table, if variables use symbol table
  - if RHS is const, otherwise pop off stack
  - if LHS is const, otherwise pop off stack
  - push result to stack
  - translate to appropriate arithmetic instruction (helper fn)
Unop:
- translate RHS to instruction
  - if RHS is constant, use const table, if variable use symbol table
  - if RHS is const, otherwise pop off stack
  - push result to stack
  - translate to appropriate arithmetic instruction
FunCall:
- move each argument into a new register
- call CALL instruction
 */
// function protocol spec:
// list of instructions
// const / symbol table
// upvalue table
// function table (TODO: do this when implementing actual nested functions)
// TODO: add variable for function before compiling its body: makes recursion possible
// TODO: add upvalues that don't exist in parent function to that parent function's upvalue table
// in case of say (in lua):
/*
local a = 1
function foo(x)
  function bar(y)
    return a
  end
end

gives
foo: upvalues = a
bar: upvalues = a
 */

// Chunk data type to store instructions and relevant info
// also functions as a sort of "state" for the codegen process
// essentially also a sort of function prototype
case class Chunk(
    instructions: List[Instruction],
    constTable: Map[Double, Int],
    symTable: Map[String, Int],
    upvalTable: Map[String, Int],
    fnTable: List[Chunk],
    paramCnt: Int,
    parent: Chunk = null
) {
  // helper functions
  def addInstructions(is: Instruction*): Chunk =
    this.copy(instructions = instructions ++ is)
  def addFn(fn: Chunk): Chunk =
    this.copy(fnTable = fnTable :+ fn)
  def addSymbol(name: String, ind: Int): Chunk =
    assert(symTable.size < 0x100, ">= 256 locals in function")
    this.copy(symTable = symTable + (name -> ind))
  def addUpval(name: String, ind: Int): Chunk =
    this.copy(upvalTable = upvalTable + (name -> ind))
  def setParent(p: Chunk): Chunk =
    this.copy(parent = p)
}

object Codegen:
  // get constant index from constant table,create new one if not present
  private inline def getConst(st: Chunk, value: Double): (Int, Chunk) =
    val consts = st.constTable
    if consts.contains(value) then (consts(value), st)
    else
      val nInd = consts.size + 0x100
      (nInd, st.copy(constTable = consts + (value -> nInd)))

  private inline def findUpval(name: String, parent: Chunk): (UpvalFlag, Int) =
    if parent == null then (UPVAL, -1)
    // in parent symbol table: return that it's local, and that it's a local in the parent
    else if parent.symTable.contains(name) then (LOCAL, parent.symTable(name))
    else if parent.upvalTable.contains(name)
    then // otherwise: check parent upvalue table
      (UPVAL, parent.upvalTable(name))
    else // non-present, add instead. TODO: check whether this works. as it stands, this MUST be coupled with an addUpval call in the parent
      (UPVAL, parent.upvalTable.size)
  // get symbol index from symbol table
  // if not present, try in upvalue table
  // if not present there, add to upvalue table
  private inline def getSym(st: Chunk, name: String): (Int, Chunk, UpvalFlag) =
    if st.symTable.contains(name) then (st.symTable(name), st, LOCAL)
    else if st.upvalTable.contains(name) then (st.upvalTable(name), st, UPVAL)
    else
      val nInd = st.upvalTable.size
      val parent = st.parent
      val (level, symind) = findUpval(name, parent)
      (nInd, st.addUpval(name, nInd), UPVAL)

  private inline def loadValue(
      tree: TreeNode,
      register: Int,
      st: Chunk
  ): Chunk =
    tree match
      case Num(n) =>
        val (constInd, st2) = getConst(st, n)
        st2.addInstructions(LOADK(register, constInd))
      case Ident(name) =>
        val (symInd, st2, flag) = getSym(st, name)
        st2.addInstructions(
          flag match
            case LOCAL => MOVE(register, symInd)
            case UPVAL => GETUPVAL(register, symInd)
        )
      case _ => processExpr(tree, st, register)

  // produce a list of psuedo-instructions (move/getupval) that indicate where the function's nth
  // upvalue is located, either as MOVE 0 X or GETUPVAL 0 X depending on whether it's local to the parent
  private inline def psuedoInstrs(fn: Chunk): List[Instruction] =
    val list = fn.upvalTable.foldLeft(IndexedSeq.fill(fn.upvalTable.size)("")) {
      case (acc, (name, ind)) => acc.updated(ind, name)
    }
    list.foldRight(List[Instruction]())((name, acc) =>
      val (flag, ind) = findUpval(name, fn.parent)
      flag match
        case UPVAL => GETUPVAL(0, ind) :: acc
        case LOCAL => MOVE(0, ind) :: acc
    )

  private inline def varAssign(
      varName: String,
      defn: TreeNode,
      register: Int,
      st: Chunk
  ): Chunk =
    // translate the definition and add to symbol table
    loadValue(defn, register, st)
      .addSymbol(varName, register)

  // returns: new state, instructions, operand value (const/reg index), register
  private inline def procssOp(
      tree: TreeNode,
      state: Chunk,
      register: Int
  ): (Chunk, Int, Int) = tree match
    case Num(value) =>
      val (constInd, st2) = getConst(state, value)
      (st2, constInd, register)
    case Ident(name) =>
      val (symInd, st2, flag) = getSym(state, name)
      flag match // if upvalue, prefix w/ getupval, otherwise dierectly use index
        case LOCAL => (st2, symInd, register)
        case UPVAL =>
          (
            st2.addInstructions(GETUPVAL(register, symInd)),
            register,
            register + 1
          )
    case _ => // arbitrary expression
      (processExpr(tree, state, register), register, register + 1)

  private inline def processFunCall(
      name: String,
      args: List[TreeNode],
      state: Chunk,
      regA: Int
  ): Chunk =
    // get register holding function prototype index
    val init = loadValue(Ident(name), regA, state)
    // process the arguments: fold w/ state, instruction list, and current register
    val (nst, _) = args.foldLeft(init, regA + 1) { case ((st, reg), arg) =>
      (processExpr(arg, st, reg), reg + 1)
    }
    nst.addInstructions(CALL(regA, args.size + 1)) // call instruction

  def processExpr(
      tree: TreeNode,
      state: Chunk,
      register: Int
  ): Chunk =
    tree match
      case BinOp(op, left, right) => // process L and R ops
        val (st2, op1, reg1) = procssOp(left, state, register)
        val (st3, op2, _) = procssOp(right, st2, reg1)
        // generate the instruction
        st3.addInstructions(
          op match
            case "+" => ADD(register, op1, op2)
            case "-" => SUB(register, op1, op2)
            case "*" => MUL(register, op1, op2)
            case "/" => DIV(register, op1, op2)
            case "%" => MOD(register, op1, op2)
            case "^" => POW(register, op1, op2)
            case _ =>
              throw Exception(
                s"invalid binary operator ${op} in expression ${tree}"
              )
        )
      case UnOp(op, right) =>
        val (st2, op1, _) = procssOp(right, state, register)
        st2.addInstructions(
          op match
            case "-" => UNM(register, op1)
            case "$" => INPUT(register, op1)
            case _ =>
              throw Exception(
                s"invalid unary operator ${op} in expression ${tree}"
              )
        )
      case FunCall(name, args) => processFunCall(name, args, state, register)
      case Num(x)              => loadValue(tree, register, state)
      case Ident(name)         => loadValue(tree, register, state)
      case _                   => throw Exception(s"invalid expression ${tree}")

  def processStmt(
      tree: TreeNode,
      state: Chunk
  ): Chunk = tree match
    case VarDef(name, value) =>
      varAssign(name, value, state.symTable.size, state)
    case VarMut(name, value) =>
      varAssign(name, value, state.symTable(name), state)
    case FunCall(name, args) =>
      processFunCall(name, args, state, state.symTable.size)
    case Return(expr) =>
      expr match
        case Ident(name) =>
          state.addInstructions(RETURN(state.symTable(name)))
        case _ =>
          val reg = state.symTable.size
          val nst = loadValue(expr, reg, state)
          nst.addInstructions(RETURN(reg))
    case FunDef(name, args, body) =>
      // add function itself as a local variable
      val parState = state.addSymbol(name, state.symTable.size)
      val map = args.zipWithIndex.toMap
      val fnState = Chunk(
        instructions = Nil,
        constTable = Map.empty,
        symTable = map,
        upvalTable = Map.empty,
        fnTable = Nil,
        paramCnt = map.size,
        parent = parState
      )
      val funbody = processExpr(body, fnState, map.size)
        .addInstructions(RETURN(map.size))

      // create closure instruction, and update state (local var + fn table)
      val afterClosure = parState
        .addFn(funbody)
        .addInstructions(
          CLOSURE(state.symTable.size, state.fnTable.size) :: psuedoInstrs(
            funbody
          ): _*
        )
      // add upvalues to parent function(s), in order of their index
      funbody.upvalTable.toList.sortBy(_._2).foldLeft(afterClosure) {
        case (st, (name, ind)) => // if upvalue not present, add to upvaltable
          if st.symTable.contains(name) || st.upvalTable.contains(name)
          then st
          else st.addUpval(name, st.upvalTable.size)
      }
    case Program(stmts) =>
      stmts.foldLeft(state) { (st, stmt) => processStmt(stmt, st) }
    case _ => throw Exception("invalid statement")

  def generate(tree: TreeNode): Chunk =
    processStmt(
      tree,
      Chunk(
        instructions = Nil,
        constTable = Map.empty,
        symTable = Map.empty,
        upvalTable = Map.empty,
        fnTable = Nil,
        paramCnt = 0,
        parent = null
      )
    )

end Codegen
