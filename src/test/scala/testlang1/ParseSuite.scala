// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
package testparser1
import TreeNode.*
// import
import munit.*
import Parse.*

class ParseSuite extends FunSuite:
  test("tokenize simple") {
    import Tokenizer.Token.*
    var tokens = Tokenizer.tokenize("1 + 1")
    assertEquals(tokens, List(Num(1), Op("+"), Num(1)))

    tokens = Tokenizer.tokenize("1 + 1 * 2")
    assertEquals(tokens, List(Num(1), Op("+"), Num(1), Op("*"), Num(2)))
  }
  test("tokenize stmt"){
    import Tokenizer.Token.*
    val tokens = Tokenizer.tokenize("let x = (1 * 3) ^ $1")
    assertEquals(tokens, List(Ident("let"), Ident("x"), Op("="), Op("("), Num(1), Op("*"), Num(3), Op(")"), Op("^"), Op("$"), Num(1)))
  }

  test("parse simple expressions") {
    val tokens = Tokenizer.tokenize("1 + 1")
    val tree = Parse.expr(tokens)._1
    assertEquals(tree, BinOp("+", Num(1), Num(1)))
  }
  test("parse simple expressions with parens") {
    val tokens = Tokenizer.tokenize("(1 + 1) * 2")
    val tree = Parse.expr(tokens)._1
    assertEquals(tree, BinOp("*",BinOp("+", Num(1), Num(1)),Num(2)))
  }
  test("parse simple expressions with parens and unary ops") {
    val tokens = Tokenizer.tokenize("(1 + 1) * -$2")
    val tree = Parse.expr(tokens)._1
    assertEquals(tree, BinOp("*",BinOp("+", Num(1), Num(1)),UnOp("-", UnOp("$",Num(2)))))
  }
  test("parse simple expressions with parens and unary ops and vars") {
    val tokens = Tokenizer.tokenize("(1 + 1) * -$x")
    val tree = Parse.expr(tokens)._1
    assertEquals(tree, BinOp("*",BinOp("+", Num(1), Num(1)),UnOp("-",UnOp("$",Ident("x")))))
  }
  test("parse simple expressions with parens and unary ops and vars and functions") {
    val tokens = Tokenizer.tokenize("(1 + 1) * -$x + f(1,2)")
    val tree = Parse.expr(tokens)._1
    assertEquals(tree, BinOp("+",BinOp("*",BinOp("+", Num(1), Num(1)),UnOp("-",UnOp("$",Ident("x")))),FunCall("f",List(Num(1), Num(2)))))
  }
  test("parse simple program w/ just return"){
    val tokens = Tokenizer.tokenize("return 1")
    val tree = Parse.parse(tokens)
    assertEquals(tree, Program(List(Return(Num(1)))))
  }
  test("parse simple program w/ vardef"){
    val tokens = Tokenizer.tokenize("let x = 1 return x")
    val tree = Parse.parse(tokens)
    assertEquals(tree, Program(List(VarDef("x", Num(1)), Return(Ident("x")))))
  }
  test("parse program with function definition"){
    val tokens = Tokenizer.tokenize("let f(x) = x return f(1)")
    val tree = Parse.parse(tokens)
    assertEquals(tree, Program(List(FunDef("f",List("x"),Ident("x")), Return(FunCall("f",List(Num(1)))))))
  }
  test("parse complex program 1"){
    val tokens = Tokenizer.tokenize("let f(x) = 2*x let a = 5+f(3) return g(1) + 1")
    val tree = Parse.parse(tokens)
    assertEquals(tree, Program(List(
      FunDef("f",List("x"),BinOp("*",Num(2),Ident("x"))),
      VarDef("a",BinOp("+",Num(5),FunCall("f",List(Num(3))))),
      Return(BinOp("+",FunCall("g",List(Num(1))),Num(1)))
    )))
  }
  test("parse variable rep in funcall and vardef"){
    val tokens = Tokenizer.tokenize("let f(x) = 2*x let x = 5+f(3) return x + 1")
    val tree = Parse.parse(tokens)
    assertEquals(tree, Program(List(
      FunDef("f",List("x"),BinOp("*",Num(2),Ident("x"))),
      VarDef("x",BinOp("+",Num(5),FunCall("f",List(Num(3))))),
      Return(BinOp("+",Ident("x"),Num(1)))
    )))
  }


