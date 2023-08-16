package testLang

object Func:
  type Upval = RegValue | (Int, Int)

  class Prototype(
      val upvalues: Int,
      val consts: Array[Double],
      val chunk: Array[Int],
      val arity: Int = 0,
      val protos: Array[Prototype] = Array.empty,
      val regs: Int
  )

  class Closure(
      val proto: Prototype,
      val upvals: Array[Upval]
  )
  def newClosure(proto: Prototype): Closure =
    Closure(proto, Array.ofDim(proto.upvalues))
