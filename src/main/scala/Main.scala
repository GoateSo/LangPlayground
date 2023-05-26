import testlang1.*

@main def foo =
  import Utils.InputType.*
  println(Utils.run(FileName, "TestPrograms/program1.blah", List(1,2,3,4,5,6,7,8,9,10)))
  println()
  println(Utils.run(FileName, "TestPrograms/program2.blah", List(1,2,3,4,5,6,7,8,9,10)))
