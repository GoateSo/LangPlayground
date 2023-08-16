package testLang

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer
import java.nio.file.Files

// either a double value or a reference to a function prototype
// avoid having tagged union associated w/ more complex types
// Double => number, Int => ptr (well more accurately, index) to function
type RegValue = Double | Int

// reads the bytecode and turns it into a nested prototype
object Reader:
  import ByteOperations.*
  import Func.*

  // parse file into nested prototypes
  def parse(bcStream: ByteArrayInputStream): Prototype =
    import ByteOperations.*
    // # of upvalues and parameters:
    val nUvals = bcStream.read()
    val nParams = bcStream.read()
    // # of registers needed
    val nRegisters = bcStream.read()
    // instructions
    val nInstrs = bcStream.readInt
    val insts = Array.ofDim[Int](nInstrs)
    (0 until nInstrs).foreach(i => insts(i) = bcStream.readInt)
    // constants
    val nConsts = bcStream.readInt
    val consts = Array.ofDim[Double](nConsts)
    (0 until nConsts).foreach(i => consts(i) = bcStream.readDouble)
    val nProtos = bcStream.readInt
    val protos = Array.ofDim[Prototype](nProtos)
    (0 until nProtos).foreach(i => protos(i) = parse(bcStream))
    Prototype(
      nUvals,
      consts,
      insts,
      nParams,
      protos,
      nRegisters
    )
  def loadfile(fileName: String) =
    import java.nio.file.{Files, Paths}
    parse(
      ByteArrayInputStream(
        Files.readAllBytes(Paths.get(fileName))
      )
    )
end Reader
