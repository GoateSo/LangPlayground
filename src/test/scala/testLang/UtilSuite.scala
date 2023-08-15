package testLang

import munit.FunSuite
import TreeNode.*
import java.io.PrintStream

class UtilSuite extends FunSuite:
  // tests for convert
  test("convert unop") {
    val results = testLang.Utils.convert(UnOp("-", Num(1))).replace(" ", "")
    assertEquals(results, "(-1.0)")
  }
  test("convert unop with parens") {
    val results =
      testLang.Utils.convert(UnOp("-", UnOp("-", Num(1)))).replace(" ", "")
    assertEquals(results, "(-(-1.0))")
  }
  test("convert binop") {
    val results =
      testLang.Utils.convert(BinOp("+", Num(1), Num(2))).replace(" ", "")
    assertEquals(results, "(1.0+2.0)")
  }
  test("convert nested binop") {
    val results = testLang.Utils
      .convert(BinOp("+", BinOp("+", Num(1), Num(2)), Num(3)))
      .replace(" ", "")
    assertEquals(results, "((1.0+2.0)+3.0)")
  }
  test("convert nested binop with parens") {
    val results = testLang.Utils
      .convert(
        BinOp("+", BinOp("+", Num(1), Num(2)), BinOp("+", Num(3), Num(4)))
      )
      .replace(" ", "")
    assertEquals(results, "((1.0+2.0)+(3.0+4.0))")
  }
  test("convert nested binop with parens and unop") {
    val results = testLang.Utils
      .convert(
        BinOp(
          "+",
          BinOp("+", Num(1), Num(2)),
          UnOp("-", BinOp("+", Num(3), Num(4)))
        )
      )
      .replace(" ", "")
    assertEquals(results, "((1.0+2.0)+(-(3.0+4.0)))")
  }
  test("convert variable definition") {
    val results = testLang.Utils.convert(VarDef("x", Num(1)))
    assertEquals(results, "let x = 1.0")
  }
  test("convert variable definition with function call") {
    val results =
      testLang.Utils.convert(VarDef("x", FunCall("f", List(Num(1)))))
    assertEquals(results, "let x = f(1.0)")
  }
  test("convert function call") {
    val results = testLang.Utils.convert(FunCall("f", List(Num(1))))
    assertEquals(results, "f(1.0)")
  }
  test("convert nested function calls") {
    val results =
      testLang.Utils.convert(FunCall("f", List(FunCall("g", List(Num(1))))))
    assertEquals(results, "f(g(1.0))")
  }
  test("convert function definition") {
    val results = testLang.Utils.convert(FunDef("f", List("x"), Num(1)))
    assertEquals(results, "let f(x) = 1.0")
  }
  test("convert variable mutation") {
    val results = testLang.Utils.convert(VarMut("x", Num(1)))
    assertEquals(results, "x = 1.0")
  }
  test("convert return statement") {
    val results = testLang.Utils.convert(Return(Num(1)))
    assertEquals(results, "return 1.0")
  }
  test("convert program") {
    val results = testLang.Utils.convert(
      Program(
        List(
          VarDef("x", Num(1)),
          VarMut("x", Num(2)),
          Return(Num(1))
        )
      )
    )
    assertEquals(results, "let x = 1.0\nx = 2.0\nreturn 1.0")
  }
  // tests for printAST
  test("printAST unop") {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      testLang.Utils.dispAST(UnOp("-", Num(1)))
    }
    val output =
      out.toString().replace("\t", " ").split("\n").map(_.strip()).toList
    assertEquals(
      output,
      """
    UnOp(-)
    | Num(1.0)
    """.strip().split("\n").map(_.strip()).toList
    )
  }
  test("printAST binop") {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      testLang.Utils.dispAST(BinOp("+", Num(1), Num(2)))
    }
    val output =
      out.toString().replace("\t", " ").split("\n").map(_.strip()).toList
    assertEquals(
      output,
      """
    BinOp(+)
    | Num(1.0)
    | Num(2.0)
    """.strip().split("\n").map(_.strip()).toList
    )
  }
  test("printAST variable definition") {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      testLang.Utils.dispAST(VarDef("x", Num(1)))
    }
    val output =
      out.toString().replace("\t", " ").split("\n").map(_.strip()).toList
    assertEquals(
      output,
      """
    VarDef(x)
    | Num(1.0)
    """.strip().split("\n").map(_.strip()).toList
    )
  }
  test("printAST variable definition with function call") {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      testLang.Utils.dispAST(VarDef("x", FunCall("f", List(Num(1)))))
    }
    val output =
      out.toString().replace("\t", " ").split("\n").map(_.strip()).toList
    assertEquals(
      output,
      """
    VarDef(x)
    | FunCall(f)
    | | Num(1.0)
    """.strip().split("\n").map(_.strip()).toList
    )
  }
  test("printAST full expression") {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      testLang.Utils.dispAST(
        BinOp(
          "+",
          BinOp("^", Num(1), Num(2)),
          UnOp("-", BinOp("*", Num(3), Num(4)))
        )
      )
    }
    val output =
      out.toString().replace("\t", " ").split("\n").map(_.strip()).toList
    assertEquals(
      output,
      """
    BinOp(+)
    | BinOp(^)
    | | Num(1.0)
    | | Num(2.0)
    | UnOp(-)
    | | BinOp(*)
    | | | Num(3.0)
    | | | Num(4.0)
    """.strip().split("\n").map(_.strip()).toList
    )
  }
  test("printAST function definition") {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      testLang.Utils.dispAST(FunDef("f", List("x"), Num(1)))
    }
    val output =
      out.toString().replace("\t", " ").split("\n").map(_.strip()).toList
    assertEquals(
      output,
      """
    FunDef(f)
    args: x
    | Num(1.0)
    """.strip().split("\n").map(_.strip()).toList
    )
  }
  test("printAST function call") {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      testLang.Utils.dispAST(FunCall("f", List(Num(1))))
    }
    val output =
      out.toString().replace("\t", " ").split("\n").map(_.strip()).toList
    assertEquals(
      output,
      """
    FunCall(f)
    | Num(1.0)
    """.strip().split("\n").map(_.strip()).toList
    )
  }
  test("printAST variable mutation") {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      testLang.Utils.dispAST(VarMut("x", Num(1)))
    }
    val output =
      out.toString().replace("\t", " ").split("\n").map(_.strip()).toList
    assertEquals(
      output,
      """
    VarMut(x)
    | Num(1.0)
    """.strip().split("\n").map(_.strip()).toList
    )
  }
  test("printAST return statement") {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      testLang.Utils.dispAST(Return(Num(1)))
    }
    val output =
      out.toString().replace("\t", " ").split("\n").map(_.strip()).toList
    assertEquals(
      output,
      """
    Return
    | Num(1.0)
    """.strip().split("\n").map(_.strip()).toList
    )
  }
  test("printAST program") {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      testLang.Utils.dispAST(
        Program(
          List(
            VarDef("x", Num(1)),
            VarMut("x", Num(2)),
            Return(Num(1))
          )
        )
      )
    }
    val output =
      out.toString().replace("\t", " ").split("\n").map(_.strip()).toList
    assertEquals(
      output,
      """
    Program
    | VarDef(x)
    | | Num(1.0)
    | VarMut(x)
    | | Num(2.0)
    | Return
    | | Num(1.0)
    """.strip().split("\n").map(_.strip()).toList
    )
  }
  test("printAST varref") {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      testLang.Utils.dispAST(
        Program(
          List(
            VarDef("x", Num(1)),
            VarMut("x", Num(2)),
            Return(Ident("x"))
          )
        )
      )
    }
    val output =
      out.toString().replace("\t", " ").split("\n").map(_.strip()).toList
    assertEquals(
      output,
      """
    Program
    | VarDef(x)
    | | Num(1.0)
    | VarMut(x)
    | | Num(2.0)
    | Return
    | | Ident(x)
    """.strip().split("\n").map(_.strip()).toList
    )
  }
