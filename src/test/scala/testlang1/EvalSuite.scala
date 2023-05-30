package testparser1

import munit.FunSuite
class EvalSuite extends FunSuite:
  test("evaluate simple expressions") {
    var ptree = Parse.expr(Tokenizer.tokenize("1 + 1"))._1
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 2.0, 0.0001)
    ptree = Parse.expr(Tokenizer.tokenize("1+2*(-3^-4+5)"))._1
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 1+2*(-math.pow(3,-4)+5), 0.0001)
  }
  test("evaluate simple program w/ just return"){
    val ptree = Parse.parse(Tokenizer.tokenize("return 1"))
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 1.0, 0.0001)
  }
  test("evaluate simple program w/ vardef"){
    val ptree = Parse.parse(Tokenizer.tokenize("let x = 1 return x"))
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 1.0, 0.0001)
  }
  test("evaluate program with function definition and call"){
    val ptree = Parse.parse(Tokenizer.tokenize("let f(x) = x return f(2)"))
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 2.0, 0.0001)
  }
  test("evaluate program with function definition and call and vardef"){
    val ptree = Parse.parse(Tokenizer.tokenize("let f(x) = x let y = f(2) return y"))
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 2.0, 0.0001)
  }
  test("evaluate program with function definition and call and vardef and varref"){
    val ptree = Parse.parse(Tokenizer.tokenize("let f(x) = x let y = f(2) return y + f(y)"))
    assertEquals(Utils.eval(ptree, Map.empty, List.empty)._1, 4.0, 0.0001)
  }
  test("evaluate program with input arguments"){
    val ptree = Parse.parse(Tokenizer.tokenize("let f(x) = x return f($1)"))
    assertEquals(Utils.eval(ptree, Map.empty, List(1,2,3))._1, 2.0, 0.0001)
  }
  test("evaluate program with non-constant input argument"){
    val ptree = Parse.parse(Tokenizer.tokenize("let f(x) = x let a = 1 return f($a) + $(1+1)"))
    assertEquals(Utils.eval(ptree, Map.empty, List(1,2,3))._1, 5.0, 0.0001)
  }

