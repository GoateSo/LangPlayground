import testparser1.*
import regexmatch.*

@main def foo =
  import Utils.InputType.*
  Utils.run(
    FileName,
    "TestPrograms/program1.blah",
    List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  println()
  Utils.run(
    FileName,
    "TestPrograms/program2.blah",
    List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  Utils.run(Prog, "let f(x) = x * x return f(2)")
  val ptree = Parser.parse(Tokenizer.tokenize("let f(x) = x * x return f(2)"))
  Utils.dispAST(TreeNode.UnOp("!", TreeNode.Num(1)))
