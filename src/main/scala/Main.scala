import TreeNode.*

def eval(
    node: TreeNode,
    env: Map[String, TreeNode],
    inputs: List[Double]
): (Double, Map[String, TreeNode]) =
  import TreeNode.*
  node match
    case node @ VarDef(name, value)      => (-1, env + (name -> node))
    case node @ FunDef(name, args, body) => (-1, env + (name -> node))
    case Num(value)                      => (value, env)
    case Return(expr)                    => eval(expr, env, inputs)
    case FunCall(name, args) =>
      env.get(name) match
        // run function adding everything to the environment, remove everything after
        case Some(FunDef(_, params, body)) =>
          var xs = env ++ params.zip(args)
          (eval(body, xs, inputs)._1, env)
        case None =>
          name match
            case "print" =>
              val values = args.map(eval(_, env, inputs)._1)
              println(values mkString "\t");
              (0, env)
            case _ => throw new Exception(s"unknown function $name")
        case _ =>
          throw Exception(
            s"non-function $name cannot be used as function in expression"
          )
    case Ident(name) =>
      env.get(name) match
        case Some(VarDef(name, value)) => eval(value, env, inputs)
        case Some(Num(value))          => (value, env)
        case None =>
          name match //
            case "PI" => (math.Pi, env)
            case "E"  => (math.E, env)
            case _    => throw Exception(s"unknown variable $name ligma")
        case Some(Ident(n)) => eval(Ident(n), env, inputs)
        case Some(x)        => eval(x, env, inputs)
    case BinOp(op, left, right) =>
      val l = eval(left, env, inputs)
      val r = eval(right, env, inputs)
      (BinopMap(op)(l._1, r._1), env)
    case UnOp(op, right) =>
      val (operand, _) = eval(right, env, inputs)
      op match
        case "-" => (-operand, env)
        case "$" if operand.isValidInt =>
          (inputs(operand.toInt), env)
        case _ =>
          throw Exception(
            s"unknown unary operator $op for given operand ${operand}"
          )
    case Program(stmts) =>
      stmts.foldLeft(0.0, env) { (acc, stmt) =>
        eval(stmt, acc._2, inputs)
      }

@main def foo =
  val tks = Tokenizer.tokenize("""
  let f(x,y) = (PI^2+x^2+y^2)^0.5
  let g = f(PI^2,4)*3
  print(g)
  print(PI,3,5)
  print(g^2)
  return g+3*$3
  """)
  println(s"tokens: ${tks.mkString("|")}")
  val ptree = Parse(tks)
  Utils.dispAST(ptree)
  println(s"${"-" * 80}\n${Utils.convert(ptree)}\n${"-" * 80} ")
  println(eval(ptree, Map.empty, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))._1)
