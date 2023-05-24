def convert(node: TreeNode): String =
  import TreeNode.*
  node match
    case BinOp(op, left, right) =>
      s"(${convert(left)} $op ${convert(right)})"
    case UnOp(op, right)     => s"($op ${convert(right)})"
    case VarDef(name, value) => s"let $name = ${convert(value)}"
    case FunDef(name, args, body) =>
      s"let $name(${args.mkString(", ")}) = ${convert(body)}"
    case FunCall(name, args) =>
      s"$name(${args.map(convert).mkString(", ")})"
    case Num(value)     => value.toString
    case Ident(name)    => name
    case Return(value)  => s"return ${convert(value)}"
    case Program(stmts) => stmts.map(convert).mkString("\n")

import TreeNode.*

def eval(node: TreeNode, env: Map[String, TreeNode], inputs : List[Double]): (Double, Map[String, TreeNode]) =
  import TreeNode.*
  node match
    case node @ VarDef(name, value)      => (-1, env + (name -> node))
    case node @ FunDef(name, args, body) => (-1, env + (name -> node))
    case FunCall(name, args)      => env.get(name) match
      case Some(FunDef(_, params, body)) => // run function adding everything to the environment, remove everything after
        var xs = env ++ params.zip(args)
        (eval(body, xs, inputs)._1 , env)
      case None => name match
        case "print" => 
          val values = args.map(eval(_, env, inputs)._1)
          println(values mkString "\t"); 
          (0, env)
        case _ => throw new Exception(s"unknown function $name")
      case _ => throw new Exception(s"non-function $name cannot be used as function in expression")
    case Num(value)               => (value, env)
    case Ident(name)              => 
      // println(env.get(name))
      env.get(name) match
      case Some(VarDef(name, value)) => eval(value, env, inputs)
      case Some(Num(value)) => (value, env)
      case None => name match // 
        case "PI" => (math.Pi, env)
        case "E"  => (math.E, env)
        case _ => throw new Exception(s"unknown variable $name ligma")
      case Some(Ident(n)) => eval(Ident(n), env, inputs)
      case Some(x) => eval(x, env, inputs)
    case BinOp(op, left, right)   => 
      val l = eval(left, env, inputs)
      val r = eval(right, env, inputs)
      (BinopMap(op)(l._1, r._1), env) 
    case UnOp(op, right)          => 
      val operand = eval(right, env, inputs)
      op match
        case "-" => (-operand._1, env)
        case "$" if operand._1.isValidInt=> 
          (inputs(operand._1.toInt), env) 
        case _   => throw new Exception(s"unknown unary operator $op for given operand ${operand._1}")
    case Return(expr)             => eval(expr, env, inputs)
    case Program(stmts)           => 
      val res = stmts.foldLeft((0.0, env)) { (acc, stmt) =>
        eval(stmt, acc._2, inputs)
      }
      res

@main def foo =
  val tks = Tokenizer.tokenize("""
  let f(x,y) = (PI^2+x^2+y^2)^0.5
  let g = f(PI^2,4)*3
  print(g)
  print(PI)
  print(g^2)
  return g+3*$3
  """)
  println(s"tokens: ${tks.mkString("|")}")
  val ptree = Parse.parse(tks)
  println(ptree)
  println(s"---------------\n${convert(ptree)}\n-------------------")
  println(eval(ptree, Map.empty, List(1,2,3,4,5,6,7,8,9,10))._1)
