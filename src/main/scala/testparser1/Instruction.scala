package testparser1

enum Instruction:
  case ADD(a: Int, b: Int, c: Int)
  case SUB(a: Int, b: Int, c: Int)
  case MUL(a: Int, b: Int, c: Int)
  case DIV(a: Int, b: Int, c: Int)
  case MOD(a: Int, b: Int, c: Int)
  case POW(a: Int, b: Int, c: Int)
  case UNM(a: Int, b: Int)
  case INPUT(a: Int, b: Int)
  case RETURN(a: Int)
  case CALL(a: Int, b: Int)
  case CLOSURE(a: Int, bx: Int)
  case GETUPVAL(a: Int, b: Int)
  case LOADK(a: Int, bx: Int)
  case MOVE(a: Int, b: Int)
