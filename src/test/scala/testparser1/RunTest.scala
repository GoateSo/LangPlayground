package testparser1

import munit.FunSuite
import java.io.OutputStream

class RunTest extends FunSuite:
  test("run program2 with File Directory") {
    import Utils.InputType.*
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      Utils.run(
        FileName,
        "TestPrograms/program2.blah",
        List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      )
    }
    val lines = out.toString().split("\n")
    val g = 3 * Math.sqrt(Math.pow(math.Pi, 2) + Math.pow(math.Pi, 4) + 16)
    assertEquals(lines(0).toDouble, g, 0.001)
    assertEquals(lines(1).split("\\s+").length, 3)
    assertEquals(lines(2).toDouble, g * g, 0.001)
    assertEquals(lines.length, 4)
    val Array(a, b) = lines(3).split("\\s+")
    assertEquals(a, "result:")
    assertEquals(b.toDouble, g + 3 * 4, 0.001)
  }
