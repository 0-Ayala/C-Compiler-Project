package csp.ch03

// From SBT: ~run-main csp.ch03.Project

object Project {
  object MyParsersNoWhitespace {
    import fastparse.all._

    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)
    val integer : Parser[Expr] = digits.map (n => CstI (n))

    val keywords : List[String] = List ("let", "in", "end")
    val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z')).!)
    val ident : Parser[String] = P ((alpha ~ (alpha | CharIn ('0' to '9')).rep (0)).!).filter (s => !keywords.contains (s))
    val qual : Parser[List[String]] = P ( ident.rep (1, sep = ".").map (s => s.toList) )
    val stringLiteral : Parser[String] = P ("\"".? ~ (alpha ~ (alpha | CharIn ('0' to '9') | CharIn (' ' to '!') | CharIn ('#' to '~') | " ").rep (0)).! ~ "\"".?) //Literally Strings. Everything.
    val variable : Parser[Expr] = ident.map(s => Var(s))

  }

  object MyParsers {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace ((" " | "\n" | "\"").rep)
    }

    import fastparse.noApi._
    import White._

    import MyParsersNoWhitespace._

    val parens : Parser[Expr] = P (integer | ("(" ~ addSub ~ ")"))
    val mult : Parser[Expr] = P (parens ~ ("*".! ~ parens).rep.map (s => s.toList)).map (foldAssocLeft)

    val typ : Parser[Unit] = P (
        ("int" | "void" | "string[]")
    )

    val atExpr : Parser[Expr] = P (
      integer |
      (ident ~ ("(" ~ expr.rep (sep = ",").map (s => s.toList) ~ ")").?).map {   
        case (nm, None)      => Var (nm)
        case (nm, Some (es)) => Call (nm, es)
      } | 
      ("(" ~ expr ~ ")") 
    )
    
    val multDiv : Parser[Expr] = P (
      ((atExpr ~ (("*" | "/" | "%").! ~ atExpr).rep.map (s => s.toList)).map (foldAssocLeft)))

    val addSub : Parser[Expr] = P (
      ((multDiv ~ (("++" | "+" | "-" ).! ~ multDiv).rep.map (s => s.toList)).map (foldAssocLeft)))

    val gtLtGeLeExpr : Parser[Expr] = P (
      ((addSub ~ (("||" | ">" | "<" | ">=" | "<=" | "!" | "&&").! ~ addSub).rep.map (s => s.toList)).map (foldAssocLeft)))

    val eqNeExpr : Parser[Expr] = P (
      ((gtLtGeLeExpr ~ (("==" | "!=").! ~ gtLtGeLeExpr).rep.map (s => s.toList)).map (foldAssocLeft)))

    val assignExpr : Parser[Expr] = P((eqNeExpr ~ ("=".! ~ eqNeExpr).rep.map (s => s.toList)).map (foldAssocLeft))

    val expr : Parser[Expr] = P (eqNeExpr)
    
    val statement : Parser[Stmt] = P ((
        ("//" ~ stringLiteral).map { case (c) => Comment()} |                                                         // In-Line Comment
        ("@" ~ CharsWhile(_ != '@').map(s=>()) ~ "@").map{case (c) => Comment()} |                                    // Block Comment
        ( typ.? ~ ident ~ "=" ~ expr ~ ";").map{case (nm, e) => Decl(nm, e)} |                                        // Reassignment
        ( typ.? ~ ident ~ "=" ~ assignExpr ~ ";").map{case (nm, e) => Decl(nm, e)} |                                  // for variable declarations
        ( expr ~ ";" ).map{case (e) => StmtExpr(e)} |                                                                 // for expressions used as statements
        ( "if" ~ "(" ~ expr ~ ")" ~ statement ~ "else".? ~ statement ).map{case (e, s1, s2) => If(e, s1, s2)} |       // if-then-else
        ( "while" ~ "(" ~ expr ~ ")" ~ statement).map{case (e, s) => While(e, s)} |                                   // while
        ("System.Console.WriteLine" ~ "(" ~ expr ~ ")" ~ ";").map{case (e) => Print(e)} |                             // Print expr
        ("System.Console.WriteLine" ~ "(" ~ (stringLiteral) ~ ")" ~ ";").map{case (s) => PrintLiteralString(s)} |     // Print LITERAL string
        (ident ~ "(" ~ expr ~ ")" ~ ";").map { case (nm, e) => FuncDef (nm, e) } |                                    // FuncDef
        ( "return " ~ expr ~ ";" ).map{case (e) => Return(e)} |                                                       // return
        //("for" ~ "(" ~ ident ~ ";" ~ expr ~ ";" ~ assignExpr ~ ")" ~ statement).map { case (nm, e1, e2, s) => For (nm, e1, e2, s) } |
       //("for" ~ "(" ~ "int" ~ ident ~ "=" ~ integer ~ ";" ~ ident.map(s => ()) ~ gtLtGeLeExpr ~ integer ~ ";" ~ statement ~ ")" ~ statement) |
        ( "{" ~ statement.rep ~ "}").map{case (ss) => Block(ss.toList)} |                                             // blocks
        (expr ~ ";").map (e => FuncCall(e))                                                                           // FuncCall
    )) 

     val method : Parser[Method] = P (
      (
        "public".? ~ "static" ~ typ ~ ident ~ "(" ~ (typ ~ ident).rep (sep = ",") ~ ")"  ~ statement ).map{case(nm, params, body) => Method(nm, params.toList, body)})
     
     val clazz : Parser[Clazz] = P (
        ("public".? ~ "class" ~ ident ~  "{" ~ method.rep ~ "}").map { case (nm, methods) => Clazz (nm, methods.toList) }
    )

     val start : Parser[Clazz] = P (clazz ~ End)                                                                     // Just to have a start and an end

  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Abstract Syntax
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  case class Method (nm: String, params: List[(String)], body : Stmt)

  case class Clazz (nm: String, methods: List[Method])    

  sealed trait Expr
  case class CstI (n : Int)                                           extends Expr
  case class CstS (s : String)                                        extends Expr
  case class Var(s: String)                                           extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr)                 extends Expr
  case class Call (nm : String, es : List[Expr])                      extends Expr
  case class Typ (s: String)                                          extends Expr

  sealed trait Stmt
  case class Decl (nm : String, e : Expr)                         extends Stmt
  case class StmtExpr (e : Expr)                                  extends Stmt
  case class If (e : Expr, s1 : Stmt, s2 : Stmt)                  extends Stmt
  case class While (e : Expr, s : Stmt)                           extends Stmt
  case class Return (e : Expr)                                    extends Stmt
  case class Block (ss : List[Stmt])                              extends Stmt
  case class Print (e : Expr)                                     extends Stmt
  case class PrintLiteralString (s : String)                      extends Stmt
  case class FuncDef (nm : String, e: Expr)                       extends Stmt
  //case class For (name: String, low: Expr, symbol: Expr, num: Expr, high: Expr, step: Expr) extends Stmt
  case class FuncCall(e: Expr)                                    extends Stmt
  case class Comment ()                                           extends Stmt


  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Parsing
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
    p match {
      case (e1, Nil) => e1
      case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
    }
  }

  import fastparse.all.{Parsed,Parser}

  //NAIVESTORE//
  type NaiveStore = Map[String,Int]

  val emptyStore : NaiveStore = Map.empty

  def getSto (store : NaiveStore, x : String) : Int = {
    store.get (x).get
  }

  def setSto (store : NaiveStore, k : String, v : Int) : NaiveStore = {
    store + ( (k, v) )
  }

  def b2i (b : Boolean) : Int = if (b) 1 else 0

  def eval (e : Expr, store : NaiveStore) : Int = {
    e match {
      case CstI (i)           => i
      //Why is Var a list of strings?
      case Var (x)            => getSto (store, x)
      case Prim (op, e1, e2) => {
        val i1 = eval (e1, store) 
        val i2 = eval (e2, store)
        op match {
          case  "+" => i1 + i2
          case  "-" => i1 - i2
          case  "*" => i1 * i2
          case "==" => b2i (i1 == i2) 
          case "<>" => b2i (i1 != i2) 
          case  "<" => b2i (i1 < i2) 
          case  ">" => b2i (i1 > i2) 
          case "<=" => b2i (i1 <= i2) 
          case ">=" => b2i (i1 >= i2) 
          case   _ => throw new RuntimeException ("unknown primitive " + op)
        }
      }
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Code Generation
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  type Env = Map[String,String]
  type FuncEnv = Map[String,Method]

  val emptyEnv : Env = Map.empty

  var labelCounter : Int = 0
  def newLabel () : String = {
    labelCounter = labelCounter + 1
    "lab%03d".format (labelCounter)
  }

  // Generate x86-64 assembly to evaluate e.
  // Result is at the top of the stack.
  // The following registers may be changed by the generated assembly language: %rax, %rbx, %rsp, %rip
  def compileExpr (e : Expr, env : Env, fenv : FuncEnv) : String = {
    e match {
      case CstI (i)           => 
        "\tpushq\t$%d\n".format (i)
      case Var (x)            => 
        env.get (x) match {
          case None => throw new RuntimeException ("unable to find variable %s in environment".format (x))
          case Some (lab) => 
            "\tpushq\t%s\n".format (lab)
        }
      case Prim (op, e1, e2) => {
        val insts1 = compileExpr (e1, env, fenv) 
        val insts2 = compileExpr (e2, env, fenv)
        val push = "\tpushq\t%rax\n"
        def pop (reg : String) = "\tpopq\t%%%s\n".format (reg)
        val instsOp : String = op match {
          case  "+" => "\taddq\t%rbx, %rax\n"
          case  "-" => "\tsubq\t%rbx, %rax\n"
          case  "*" => "\timulq\t%rbx, %rax\n"
          case  "=" => {
            "\tcmpq\t%rbx, %rax\n" +    // sets ZF if ((rax-rbx) = 0) as signed, i.e., (rax = rbx)
            "\tsete\t%al\n" +           // sets low-order byte (%al) of %rax to 1 if ZF is set, otherwise to 0
            "\tmovzbl\t%al, %eax\n"     // extends %al to %rax (recall that assignment to a 32-bit register clears the upper 32-bits of the corresponding 64-bit register)
          }
          case "/" => {
            "\tcltd\n" +
            "\tidivq\t%rbx\n"
          }
          case "%" => {
            "\tcltd\n" +
            "\tidivq\t%rbx\n" +
            "\tmovq\t%rdx,\t%rax\n"
          }

          case "&&" => {
                "\tmovl\t8(%esp), %edx\n" +
                "\txorl\t%eax, %eax\n" +
                "\tmovl\t4(%esp), %ecx\n" +  
                "\ttestl\t%edx, %edx\n" + 
                "\tsetne\t%al\n" +
                "\txorl\t%edx, %edx\n" + 
                "\ttestl\t%ecx, %ecx\n" +
                "\tsetne\t%dl\n" +
                "\tandl\t%edx, %eax\n"
          }
          case "==" => {
            "\tcmpq\t%rbx, %rax\n" +
            "\tsete\t%al\n" +
            "\tmovzbl\t%al, %eax\n"
          }
          case "!=" => {
            "\tcmpq\t%rbx, %rax\n" +
            "\tsetne\t%al\n" +
            "\tmovzbl\t%al, %eax\n"
          }
          case "<=" => {
            "\tcmpq\t%rbx, %rax\n" +
            "\tsetle\t%al\n" +
            "\tmovzbl\t%al, %eax\n"
          }

          // case "<>" => b2i (i1 != i2) 
          case  "<" => {
            "\tcmpq\t%rbx, %rax\n" +    // sets SF if ((rax-rbx) < 0) as signed, i.e., (rax < rbx)
            "\tsets\t%al\n" +           // sets low-order byte (%al) of %rax to 1 if SF is set, otherwise to 0
            "\tmovzbl\t%al, %eax\n"     // extends %al to %rax (recall that assignment to a 32-bit register clears the upper 32-bits of the corresponding 64-bit register)
          }
          // case  ">" => b2i (i1 > i2) 
          // case "<=" => b2i (i1 <= i2) 
          // case ">=" => b2i (i1 >= i2) 
          case   _ => throw new RuntimeException ("unknown primitive " + op)
        }
        insts1 +
        insts2 +
        pop ("rbx") +
        pop ("rax") +
        instsOp + 
        push
      }
      case Call (nm, es) => {
        es.reverse.map (e => compileExpr (e, env, fenv)).mkString +
        "\tcall\t%s\n".format (nm) + 
        "\taddq\t$%d, %%rsp\n".format (es.length * 8) +
        "\tpushq\t%rax\n"
      }
    }
  }

  def compileAll (clazz: Clazz, env : Env, fenv : FuncEnv) : String = {
    header () + 
    "\n" +
    clazz.methods.map (fd => compileFunc (fd, env, fenv)).mkString ("\n") + 
    footer (env)
  }

  def header () : String = {
    ""
  }

  def footer (env : Env) : String = {
    "\n" +
    "\t.section .rodata\n" + 
    ".output:\n" + 
    "\t.string \"%d\\n\"\n" +
    "\n" +
    (for ((nm1, _) <- env) yield {
      "\t.globl\t%s\n".format (nm1) +
      "\t.data\n".format (nm1) +
      "\t.align\t8\n" +
      "\t.size\t%s, 8\n".format (nm1) +
      "%s:\n".format (nm1) +
      "\t.quad\t0\n" +
      "\n"
    }).mkString
  }

  def compileFunc (methods: Method, env : Env, fenv : FuncEnv) : String = {
    val header = {
      "\t.text\n" +
      "\t.globl\t%s\n".format (methods.nm) +
      "\t.type\t%s, @function\n".format (methods.nm) +
      "%s:\n".format (methods.nm) + 
      "\tpushq\t%rbp\n" + 
      "\tmovq\t%rsp, %rbp\n" 
    }
    val footer = {
      "\tpopq\t%rbp\n" + 
      "\tret\n"
    }
    var env2 : Env = env
    for ((param, i) <- methods.params.zipWithIndex) {
      env2 = env2 + ( (param, "%d(%%rbp)".format ((i + 2) * 8)) ) 
    }
    header + 
    compileStmt (methods.body, env2, fenv) + 
    footer
  }

  def compileStmt (s : Stmt, env : Env, fenv : FuncEnv) : String = {
    s match {
      case Decl (nm, e)            => {
        env.get (nm) match {
          case None => throw new RuntimeException ("unable to find variable %s in environment".format (nm))
          case Some (lab) => 
            compileExpr (e, env, fenv) + 
            "\tpopq\t%rax\n" +
            "\tmovq\t%%rax, %s\n".format (lab)
      }
    }
      case If (e, s1, s2)          => {
        val label1 = newLabel ()
        val label2 = newLabel ()
        val label3 = newLabel () 
        compileExpr (e, env, fenv) +
        "\tpopq\t%rax\n" + 
        "\ttestq\t%rax, %rax\n" + 
        "\tjne\t%s\n".format (label1) +
        "\tjmp\t%s\n".format (label2) +
        "%s:\n".format (label1) +
        compileStmt (s1, env, fenv) +
        "\tjmp\t%s\n".format (label3) +
        "%s:\n".format (label2) +
        compileStmt (s2, env, fenv) +
        "%s:\n".format (label3) 
      }
      case Block (ss)              => {
        def loop (ss2 : List[Stmt]) : String = {
          ss2 match {
            case Nil       => ""
            case s2 :: ss3 => compileStmt (s2, env, fenv) + loop (ss3)
          }
        }
        loop (ss)
      }
      // case For (nm, low, high, s)  => {
      //   val label1 = newLabel ()
      //   val label2 = newLabel ()
      //   "// for (%s := %s to %s)\n".format (nm, ppExpr (low), ppExpr (high)) +
      //   compileExpr (low, env, fenv) + 
      //   "\tpopq\t%rax\n" + 
      //   "\tmovq\t%%rax, (%s)\n".format (nm) +
      //   "\tjmp\t%s\n".format (label2) +
      //   "%s:\n".format (label1) +
      //   compileStmt (s, env, fenv) +
      //   "\tmovq\t(%s), %%rax\n".format (nm) +
      //   "\taddq\t$1, %rax\n" +
      //   "\tmovq\t%%rax, (%s)\n".format (nm) +
      //   "%s:\n".format (label2) +
      //   compileExpr (high, env, fenv) + 
      //   "\tpopq\t%rbx\n" + 
      //   "\tmovq\t(%s), %%rax\n".format (nm) +
      //   "\tcmpq\t%rbx, %rax\n" + 
      //   "\tjle\t%s\n".format (label1)
      // }
      case While (e, s)            => {
        val label1 = newLabel ()
        val label2 = newLabel ()
        "\tjmp\t%s\n".format (label2) +
        "%s:\n".format (label1) +
        compileStmt (s, env, fenv) +
        "%s:\n".format (label2) +
        compileExpr (e, env, fenv) + 
        "\tpopq\t%rax\n" + 
        "\ttestq\t%rax, %rax\n" + 
        "\tjne\t%s\n".format (label1)
      }
      case Print (e)               => {
        compileExpr (e, env, fenv) +
        "\tpopq\t%rsi\n" +
        "\tmovl\t$.output, %edi\n" + 
        "\tmovl\t$0, %eax\n" +
        "\tcall\tprintf\n"
      }
      case PrintLiteralString (s)               => {
          val label = newLabel()
          "\t.section\t.rodata\n" +
          "%s:\n".format(label) + 
          "\t.string\t\"%s\"\n".format(s) +
          "\t.text\n" +
          "\tmovl\t$%s, %%edi\n".format(label) +
          "\tcall\tputs\n"
      }
      case Return (e)               => { 
        compileExpr (e, env, fenv) +
        "\tpopq\t%rax\n" +
        "\tpopq\t%rbp\n" + 
        "\tret\n"
      }
      case FuncCall(e) => {
        compileExpr(e, env, fenv)
      }

      case Comment () => {""}
    }
  }

  def findVarsExpr (e : Expr) : List[String] = {
    e match {
      case CstI (i)           => Nil
      case Var (x)            => List (x)
      case Prim (op, e1, e2)  => findVarsExpr (e1) ::: findVarsExpr (e2)
      case Call (nm, es)      => es.flatMap (findVarsExpr)
    }
  }

  def findVarsStmt (s : Stmt) : List[String] = {
    s match {
      case Decl (nm, e)            => nm :: findVarsExpr (e)
      case If (e, s1, s2)          => findVarsExpr (e) ::: findVarsStmt (s1) ::: findVarsStmt (s2)
      case Block (ss)              => {
        def loop (ss2 : List[Stmt]) : List[String] = {
          ss2 match {
            case Nil       => Nil
            case s2 :: ss3 => findVarsStmt (s2) ::: loop (ss3)
          }
        }
        loop (ss)
      }
      // case For (nm, low, high, s)  => {
      //   nm :: findVarsExpr (low) ::: findVarsExpr (high) ::: findVarsStmt (s)
      // }
      case While (e, s)            => {
        findVarsExpr (e) ::: findVarsStmt (s)
      }
      case PrintLiteralString(s) => {
        List()
      }
      case Return (e)              => {
        findVarsExpr (e)
          }
      case StmtExpr (e) => {
        findVarsExpr(e)
      }

      case Print(e) => {
        findVarsExpr(e)
      }

      case FuncCall(e) => {
        findVarsExpr(e)
      }

      case Comment()  => {
        List()
      }
      
    }
  }

  def findVars (s : Stmt) : List[String] = {
    findVarsStmt (s).toSet.toList.sortWith ((s1,s2) => s1 < s2)
  }

    def compile (clazz : Clazz, filename: String) : Unit = {
      val menv : FuncEnv = (for (md <- clazz.methods) yield (md.nm, md)).toMap
      val vars : List[String] = for (stmt <- (clazz.methods.map (m => m.body)); v <- findVars (stmt)) yield v
      val env : Env = (for (v <- vars) yield (v, "(%s)".format (v))).toMap
      println ("Variables: %s".format (env.mkString (", ")))
      println ("Compiling:")
      val asm : String = compileAll (clazz, env, menv)
      
      val asmFilename = filename.replace (".cs", ".s")
      val fw = new java.io.FileWriter (asmFilename)
      fw.write (asm)
      fw.close
      println ("Wrote to %s".format (asmFilename))
      //invokeAssemblerLinker (asmFilename)
      println (asm)
  }

  def readFile (filename : String) : String = {
    val source : scala.io.BufferedSource = io.Source.fromFile (filename)
    try source.getLines.mkString ("\n") finally source.close ()
  }

  def test (p : Parser[Clazz], filename : String) : Unit = {
    val input : String = readFile (filename)
    val result : fastparse.core.Parsed[Clazz, Char, String] = p.parse (input) 
    result match {
      case Parsed.Success (prog, successIndex) => {
        println ("Successfully parsed file \"%s\".\nResult is %s.\nIndex is %d.".format (filename, prog, successIndex))
        // println ("Pretty printing:")
        // print (ppStmt ("  ", stmt))
        compile (prog, filename)
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse file \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (filename, lastParser, index, extra))
      }
    }
  }

  def main (args : Array[String]) {
    println ("=" * 80)
    
    import java.io.File
    for (f <- new File ("./input").listFiles.toList.sortWith ((f1, f2) => f1.getName < f2.getName);
         if (f.getName.endsWith (".cs"))) {
      test (MyParsers.start, f.getPath)
      println ("=" * 80)
    }
  }
}