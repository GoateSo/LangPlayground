package testparser1

import munit.FunSuite
class EvalSuite extends FunSuite:
  test("evaluate simple expressions") {
    var ptree = Parser.expr(Tokenizer.tokenize("1 + 1"))._1
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 2.0, 0.0001)
    ptree = Parser.expr(Tokenizer.tokenize("1+2*(-3^-4+5)"))._1
    assertEquals(
      Utils.eval(ptree, Map.empty, List.empty)._1,
      1 + 2 * (-math.pow(3, -4) + 5),
      0.0001
    )
  }
  test("evaluate simple program w/ just return") {
    val ptree = Parser.parse(Tokenizer.tokenize("return 1"))
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 1.0, 0.0001)
  }
  test("evaluate simple program w/ vardef") {
    val ptree = Parser.parse(Tokenizer.tokenize("let x = 1 return x"))
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 1.0, 0.0001)
  }
  test("evaluate program with function definition and call") {
    val ptree = Parser.parse(Tokenizer.tokenize("let f(x) = x return f(2)"))
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 2.0, 0.0001)
  }
  test("evaluate program with function definition and call and vardef") {
    val ptree =
      Parser.parse(Tokenizer.tokenize("let f(x) = x let y = f(2) return y"))
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 2.0, 0.0001)
  }
  test(
    "evaluate program with function definition and call and vardef and varref"
  ) {
    val ptree = Parser.parse(
      Tokenizer.tokenize("let f(x) = x let y = f(2) return y + f(y)")
    )
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 4.0, 0.0001)
  }
  test("evaluate program with input arguments") {
    val ptree = Parser.parse(Tokenizer.tokenize("let f(x) = x return f($1)"))
    assertEquals(Utils.eval(ptree, Map.empty, List(1, 2, 3))._1, 2.0, 0.0001)
  }
  test("evaluate program with non-constant input argument") {
    val ptree = Parser.parse(
      Tokenizer.tokenize("let f(x) = x let a = 1 return f($a) + $(1+1)")
    )
    assertEquals(Utils.eval(ptree, Map.empty, List(1, 2, 3))._1, 5.0, 0.0001)
  }
  test("evaluate program with variable mutations") {
    val ptree = Parser.parse(Tokenizer.tokenize("let x = 1 x = 2 return x"))
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 2.0, 0.0001)
  }
  test("evaluate program with global var usage") {
    import TreeNode.*
    val ptree = Parser.parse(Tokenizer.tokenize("return PI + E"))
    assertEquals(
      Utils
        .eval(ptree, Map("PI" -> Num(math.Pi), "E" -> Num(math.E)), List.empty)
        ._1,
      math.Pi + math.E,
      0.0001
    )
  }
  test("catch error with unknown binop") {
    import TreeNode.*
    val ptree = BinOp("\\", Num(1), Num(1))
    intercept[Exception] {
      Utils.eval(ptree, Map.empty, List.empty)
    }
  }
  test("catch error with unknown unop") {
    import TreeNode.*
    val ptree = UnOp("!", Num(1))
    intercept[Exception] {
      Utils.eval(ptree, Map.empty, List.empty)
    }
  }
  test("catch error with unknown variable use") {
    val ptree = Parser.parse(Tokenizer.tokenize("let x = 1 return y"))
    intercept[Exception] {
      Utils.eval(ptree, Map.empty, List.empty)
    }
  }
  test("catch error with unknown function use") {
    val ptree = Parser.parse(Tokenizer.tokenize("return f(1)"))
    assertEquals(intercept[Exception] {
      Utils.eval(ptree, Map.empty, List.empty)
    }.getMessage(), "unknown function f")
  }
  test("catch error with unknown param use") {
    val ptree = Parser.parse(Tokenizer.tokenize("let f(x) = 1 return f(y)"))
    intercept[Exception] {
      Utils.eval(ptree, Map.empty, List.empty)
    }
  }

