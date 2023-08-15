// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
package testLang
import TreeNode.*
// import
import munit.*
import Parser.*
import testLang.TreeNode
import testLang.Parser

class ParseSuite extends FunSuite:
  test("tokenize simple") {
    import testLang.Tokenizer.Token.*
    var tokens = testLang.Tokenizer.tokenize("1 +  1")
    assertEquals(tokens, List(Num(1), Op("+"), Num(1)))

    tokens = testLang.Tokenizer.tokenize("1 + 1 * 2")
    assertEquals(tokens, List(Num(1), Op("+"), Num(1), Op("*"), Num(2)))
  }
  test("tokenize numeric fail") {
    val msg = intercept[Exception] {
      testLang.Tokenizer.tokenize("1.2.3")
    }.getMessage()
    assert(msg.contains("Invalid number, too many decimal points"))
  }
  test("tokenize stmt") {
    import testLang.Tokenizer.Token.*
    val tokens = testLang.Tokenizer.tokenize("let x = (1 * 3) ^ $1")
    assertEquals(
      tokens,
      List(
        Ident("let"),
        Ident("x"),
        Op("="),
        Op("("),
        Num(1),
        Op("*"),
        Num(3),
        Op(")"),
        Op("^"),
        Op("$"),
        Num(1)
      )
    )
  }

  test("parse simple expressions") {
    val tokens = testLang.Tokenizer.tokenize("1 + 1")
    val tree = testLang.Parser.expr(tokens)._1
    assertEquals(tree, BinOp("+", Num(1), Num(1)))
  }
  test("parse simple expressions with parens") {
    val tokens = testLang.Tokenizer.tokenize("(1 + 1) * 2")
    val tree = testLang.Parser.expr(tokens)._1
    assertEquals(tree, BinOp("*", BinOp("+", Num(1), Num(1)), Num(2)))
  }
  test("parse simple expressions with parens and unary ops") {
    val tokens = testLang.Tokenizer.tokenize("(1 + 1) * -$2")
    val tree = testLang.Parser.expr(tokens)._1
    assertEquals(
      tree,
      BinOp("*", BinOp("+", Num(1), Num(1)), UnOp("-", UnOp("$", Num(2))))
    )
  }
  test("parse simple expressions with parens and unary ops and vars") {
    val tokens = testLang.Tokenizer.tokenize("(1 + 1) * -$x")
    val tree = testLang.Parser.expr(tokens)._1
    assertEquals(
      tree,
      BinOp("*", BinOp("+", Num(1), Num(1)), UnOp("-", UnOp("$", Ident("x"))))
    )
  }
  test(
    "parse simple expressions with parens and unary ops and vars and functions"
  ) {
    val tokens = testLang.Tokenizer.tokenize("(1 + 1) * -$x + f(1,2)")
    val tree = testLang.Parser.expr(tokens)._1
    assertEquals(
      tree,
      BinOp(
        "+",
        BinOp(
          "*",
          BinOp("+", Num(1), Num(1)),
          UnOp("-", UnOp("$", Ident("x")))
        ),
        FunCall("f", List(Num(1), Num(2)))
      )
    )
  }
  test("parse simple program w/ just return") {
    val tokens = testLang.Tokenizer.tokenize("return 1")
    val tree = testLang.Parser.parse(tokens)
    assertEquals(tree, Program(List(Return(Num(1)))))
  }
  test("parse simple program w/ vardef") {
    val tokens = testLang.Tokenizer.tokenize("let x = 1 return x")
    val tree = testLang.Parser.parse(tokens)
    assertEquals(tree, Program(List(VarDef("x", Num(1)), Return(Ident("x")))))
  }
  test("parse program with function definition") {
    val tokens = testLang.Tokenizer.tokenize("let f(x) = x return f(1)")
    val tree = testLang.Parser.parse(tokens)
    assertEquals(
      tree,
      Program(
        List(
          FunDef("f", List("x"), Ident("x")),
          Return(FunCall("f", List(Num(1))))
        )
      )
    )
  }
  test("parse complex program 1") {
    val tokens =
      testLang.Tokenizer.tokenize(
        "let f(x) = 2*x let a = 5+f(3) return g(1) + 1"
      )
    val tree = testLang.Parser.parse(tokens)
    assertEquals(
      tree,
      Program(
        List(
          FunDef("f", List("x"), BinOp("*", Num(2), Ident("x"))),
          VarDef("a", BinOp("+", Num(5), FunCall("f", List(Num(3))))),
          Return(BinOp("+", FunCall("g", List(Num(1))), Num(1)))
        )
      )
    )
  }
  test("parse variable rep in funcall and vardef") {
    val tokens =
      testLang.Tokenizer.tokenize("let f(x) = 2*x let x = 5+f(3) return x + 1")
    val tree = testLang.Parser.parse(tokens)
    assertEquals(
      tree,
      Program(
        List(
          FunDef("f", List("x"), BinOp("*", Num(2), Ident("x"))),
          VarDef("x", BinOp("+", Num(5), FunCall("f", List(Num(3))))),
          Return(BinOp("+", Ident("x"), Num(1)))
        )
      )
    )
  }
  test("parse multi-param function def and call") {
    val tokens =
      testLang.Tokenizer.tokenize("let f(x,y) = x*y return f(1+1,2+2)")
    val tree = testLang.Parser.parse(tokens)
    assertEquals(
      tree,
      Program(
        List(
          FunDef("f", List("x", "y"), BinOp("*", Ident("x"), Ident("y"))),
          Return(
            FunCall(
              "f",
              List(BinOp("+", Num(1), Num(1)), BinOp("+", Num(2), Num(2)))
            )
          )
        )
      )
    )
  }
  test("parse variable operations (def, assign, use)") {
    val tokens = testLang.Tokenizer.tokenize("let x = 1 x = x + 1 return x")
    val tree = testLang.Parser.parse(tokens)
    assertEquals(
      tree,
      Program(
        List(
          VarDef("x", Num(1)),
          VarMut("x", BinOp("+", Ident("x"), Num(1))),
          Return(Ident("x"))
        )
      )
    )
  }
  test("parse function call as statement") {
    val tokens = testLang.Tokenizer.tokenize("f(1)")
    val tree = testLang.Parser.parse(tokens)
    assertEquals(tree, Program(List(FunCall("f", List(Num(1))))))
  }
  test("catch error on malformed arglist") {
    val tokens = testLang.Tokenizer.tokenize("f(1,2,3")
    val msg = intercept[Exception] {
      testLang.Parser.parse(tokens)
    }.getMessage()
    // println(msg)
    assert(msg.contains("expected ',' or ')' in function call"))
  }
  test("catch error on malformed arglist 2") {
    val tokens = testLang.Tokenizer.tokenize("f(1,2,3,)")
    val msg = intercept[Exception] {
      testLang.Parser.parse(tokens)
    }.getMessage()
    // println(msg
    assert(msg.contains("expected expression after ','"))
  }
  test("catch error in malformed expression") {
    val tokens = testLang.Tokenizer.tokenize("1 + ")
    intercept[Exception] {
      testLang.Parser.parse(tokens)
    }
  }
  test("catch error in malformed vardef (no body)") {
    val tokens = testLang.Tokenizer.tokenize("let x = ")
    intercept[Exception] {
      testLang.Parser.parse(tokens)
    }
  }
  test("catch error in malformed vardef 2 (no '=')") {
    val tokens = testLang.Tokenizer.tokenize("let x 1 + ")
    intercept[Exception] {
      testLang.Parser.parse(tokens)
    }
  }
  test("catch error in malformed vardef 3 (no name)") {
    val tokens = testLang.Tokenizer.tokenize("let = 1 + ")
    intercept[Exception] {
      testLang.Parser.parse(tokens)
    }
  }
  test("catch error in malformed fundef (no body)") {
    val tokens = testLang.Tokenizer.tokenize("let f(x) = ")
    intercept[Exception] {
      testLang.Parser.parse(tokens)
    }
  }
  test("catch error in malformed fundef 2 (no '=')") {
    val tokens = testLang.Tokenizer.tokenize("let f(x) 1 + x")
    intercept[Exception] {
      testLang.Parser.parse(tokens)
    }
  }
  test("catch error in malformed fundef 3 (no name)") {
    val tokens = testLang.Tokenizer.tokenize("let (x) = 1 + x")
    intercept[Exception] {
      testLang.Parser.parse(tokens)
    }
  }
