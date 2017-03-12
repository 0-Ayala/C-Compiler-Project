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
    val stringLiteral : Parser[String] = P ("\"".? ~ (alpha ~ (alpha | CharIn ('0' to '9') | " ").rep (0)).! ~ "\"".?)
    // val literal_string : Parser[String] = P ("\"" ~ (CharIn (' ' to '!') |
    //                                                  CharIn ('#' to '~')
    //                                                  ).rep ().! ~ "\"" )
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
      variable |
      (qual ~ ("(" ~ expr.rep (sep = ",").map (s => s.toList) ~ ")").?).map {   
        case (nm, None)      => NVar (nm)
        case (nm, Some (es)) => Call (nm, es)
      } | 
      ("(" ~ expr ~ ")") 
    )
    

    val multDiv : Parser[Expr] = P (
      ((atExpr ~ (("*" | "/" | "*=").! ~ atExpr).rep.map (s => s.toList)).map (foldAssocLeft)))

    val addSub : Parser[Expr] = P (
      ((multDiv ~ (("+" | "++" | "-" | "%").! ~ multDiv).rep.map (s => s.toList)).map (foldAssocLeft)))

    val gtLtGeLeExpr : Parser[Expr] = P (
      ((addSub ~ (("||" | ">" | "<" | ">=" | "<=" | "!" | "&&").! ~ addSub).rep.map (s => s.toList)).map (foldAssocLeft)))

    val eqNeExpr : Parser[Expr] = P (
      ((gtLtGeLeExpr ~ (("==" | "!=").! ~ gtLtGeLeExpr).rep.map (s => s.toList)).map (foldAssocLeft)))

    val assignExpr : Parser[Expr] = P((eqNeExpr ~ ("=".! ~ eqNeExpr).rep.map (s => s.toList)).map (foldAssocLeft))

    val expr : Parser[Expr] = P (eqNeExpr)

    val statement : Parser[Stmt] = P ((
        ( typ.? ~ ident ~ "=" ~ expr ~ ";").map{case (nm, e) => Decl(nm, e)} |
        ( typ.? ~ ident ~ "=" ~ assignExpr ~ ";").map{case (nm, e) => Decl(nm, e)} |                                  // for variable declarations
        ( expr ~ ";" ).map{case (e) => StmtExpr(e)} |                                                           // for expressions used as statements
        ( "if" ~ "(" ~ expr ~ ")" ~ statement ~ "else".? ~ statement ).map{case (e, s1, s2) => If(e, s1, s2)} |   // if-then-else
        ( "while" ~ "(" ~ expr ~ ")" ~ statement).map{case (e, s) => While(e, s)} |                            // while
        ("System.Console.WriteLine" ~ "(" ~ stringLiteral ~ ")" ~ ";").map{case (s) => PrintLiteralString(s)} |
        (ident ~ "(" ~ expr ~ ")" ~ ";").map { case (nm, e) => FuncDef (nm, e) } |
        ( "return " ~ expr ~ ";" ).map{case (e) => Return(e)} |                                             // return
        ( "{" ~ statement.rep ~ "}").map{case (ss) => Block(ss.toList)}                                         // blocks
    )) 

     val method : Parser[Method] = P (
      (
        "public".? ~ "static" ~ typ ~ ident ~ "(" ~ (typ ~ ident).rep (sep = ",") ~ ")"  ~ statement ).map{case(nm, params, body) => Method(nm, params.toList, body)})
     
     val clazz : Parser[Clazz] = P (
        ("public" ~ "class" ~ ident ~  "{" ~ method.rep ~ "}").map { case (nm, methods) => Clazz (nm, methods.toList) }
    )

  }
  //Why does body have to be a list of Statements even though Statements has block which treats a bunch of statements as a list of statements?
  case class Method (nm: String, params: List[(String)], body : Stmt)

  case class Clazz (nm: String, methods: List[Method])    

      // These definitions for parsing expressions are adapted from NaiveCodeGenFunc.scala
  sealed trait Expr
  case class CstI (n : Int)                                           extends Expr
  case class CstS (s : String)                                        extends Expr
  case class NVar (nms : List[String])                                 extends Expr
  case class Var(s: String)                                          extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr)                 extends Expr
  case class Call (nm : List[String], es : List[Expr])                extends Expr
  case class Typ (s: String)                                          extends Expr


  sealed trait Stmt
  case class Decl (nm : String, e : Expr)                         extends Stmt
  case class RDecl (nm: String, e: Expr)                          extends Stmt
  case class StmtExpr (e : Expr)                                  extends Stmt
  case class If (e : Expr, s1 : Stmt, s2 : Stmt)                  extends Stmt
  case class While (e : Expr, s : Stmt)                           extends Stmt
  case class Return (e : Expr)                                    extends Stmt
  case class Block (ss : List[Stmt])                              extends Stmt
  case class PrintLiteralString (s : String)                      extends Stmt
  case class FuncDef (nm : String, e: Expr)                       extends Stmt

  def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
    p match {
      case (e1, Nil) => e1
      case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
    }
  }

  import fastparse.all.{Parsed,Parser}

  def test (p : Parser[Clazz], s : String) : Unit = {
    val result : fastparse.core.Parsed[Clazz, Char, String] = p.parse (s) 
    result match {
      case Parsed.Success (value, successIndex) => {
        println ("Successfully parsed \"%s\".  Result is %s.  Index is %d.".format (s, value, successIndex))
      compile(value)
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (s, lastParser, index, extra))
      }
    }
  }

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

  def exec (s : Stmt, store : NaiveStore) : NaiveStore = {
    s match {
      case Decl(nm, e)            => {
        val v : Int = eval (e, store)
        // println ("store is %s".format (store))
        // println ("assigning %d to %s".format (v, nm))
        setSto (store, nm, v)
      }
      case If (e, s1, s2)          => exec (if (eval (e, store) != 0) s1 else s2, store)
      case Block (ss)              => {
        def loop (ss2 : List[Stmt], store2 : NaiveStore) : NaiveStore = {
          ss2 match {
            case Nil       => store2
            case s2 :: ss3 => loop (ss3, exec (s2, store2))
          }
        }
        loop (ss, store)
      }
      // case For (nm, low, high, s)  => {
      //   val start : Int = eval (low, store) 
      //   val stop : Int = eval (high, store)
      //   def loop (i : Int, sto : NaiveStore) : NaiveStore = {
      //     if (i > stop) {
      //       sto 
      //     } else {
      //       loop (i + 1, exec (s, setSto (sto, nm, i)))
      //     }
      //   }
      //   loop (start, store)
      // }
      case While (e, s)            => {
        def loop (sto : NaiveStore) : NaiveStore = {
          if (eval (e, sto) != 0) {
            loop (exec (s, sto))
          } else {
            sto
          }
        }
        loop (store)
      }
      // case Print (s)               => {
      //   println ((s, store))
      //   store
      // }

      // case statement (e)               => {
      //   println (eval (e, store))
      //   store
      // }
    }
  }


 // val helloWorld : Stmt = {
 //    Block (
 //      List (
 //        Print("hello world")
 //      )
 //    )
 //  }


  // val factorial : Stmt = {
  //     Block (
  //       List (
  //         statement (
  //           "fact", 
  //           CstI (1)
  //         ),
  //         For (
  //           "i", 
  //           CstI (1), 
  //           CstI (10), 
  //           statement (
  //             "fact", 
  //             Prim ("*", Var ("fact"), Var ("i"))
  //           )
  //         ),
  //         statement (Var ("fact"))
  //       )
  //     )
  //   }

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
                "\tmovl    \t8(%esp), \t%edx" +
                "\txorl    \t%eax, \t%eax" +
                "\tmovl    \t4(%esp), \t%ecx" +  
                "\ttestl   \t%edx, \t%edx" + 
                "\tsetne   \t%al" +
                "\txorl    \t%edx, \t%edx" + 
                "\ttestl   \t%ecx, \t%ecx" +
                "\tsetne   \t%dl" +
                "\tandl    \t%edx, \t%eax"
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
    //compileFunc (Method (nm, params, body), env, fenv) + 
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

    // def compileStr (s : String, env : Env, fenv : FuncEnv) : String = {
    // s match {
    //     case PrintLiteralString(s) => {List()}
    //     }
    //   }
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
      case PrintLiteralString (s)               => {
        "\t.section\t.rodata\n" + 
        "LC0:\n" +
        "\t.string\t\"%s\n".format (s) +
        "\t.text\n" +
        "\tpopq\t%rsi\n" +
        "\tmovl\t$.output, %edi\n" + 
        "\tmovl\t$0, %eax\n" +
        "\tcall\tprintf\n"
      }
      case Return (e)               => { 
        compileExpr (e, env, fenv) +
        "\tpopq\t%rax\n" +
        "\tpopq\t%rbp\n" + 
        "\tret\n"
      }
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
    }
  }

  def findVars (s : Stmt) : List[String] = {
    findVarsStmt (s).toSet.toList.sortWith ((s1,s2) => s1 < s2)
  }

    def compile (clazz : Clazz) : Unit = {
    val menv : FuncEnv = (for (md <- clazz.methods) yield (md.nm, md)).toMap
    val vars : List[String] = for (stmt <- (clazz.methods.map (m => m.body)); v <- findVars (stmt)) yield v
    val env : Env = (for (v <- vars) yield (v, "(%s)".format (v))).toMap
    println ("Variables: %s".format (env.mkString (", ")))
    println ("Compiling:")
    val asm : String = compileAll (clazz, env, menv)
    //val asmFilename = filename.replace (".cs", ".s")
    //val fw = new java.io.FileWriter (asmFilename)
    // fw.write (asm)
    // fw.close
    // println ("Wrote to %s".format (asmFilename))
    // //invokeAssemblerLinker (asmFilename)
    println (asm)
  }

  def main (args : Array[String]) {
    println ("=" * 80)

    val p01 : Parser[Clazz] = MyParsers.clazz

    test (p01, """public class Program
    {
      static void Main(int args) {
        System.Console.WriteLine("Hello world");
    }    
    }
""")

    println ("=" * 80)

    test (p01, """public class Program {
    static void Main(){
      int accumulator = 1;
      int factor = 1;
      int number = 10;
      while (factor < number){
        accumulator = accumulator * factor;
        factor = factor + 1;

      }
      System.Console.WriteLine(accumulator);

    }
}""")

    println ("=" * 80)

    val p02 : Parser[Clazz] = MyParsers.clazz

    test (p02, """public class Program {
      static void Main(){
        int number = 0;
        while(number < 101){
            System.Console.WriteLine(number);
            number = number + 1;
          }
       }
    

    }""")

    println ("=" * 80)

    val p03 : Parser[Clazz] = MyParsers.clazz

    test (p03, """public class Program {
      static void Main(){
        int count = 0;
        int number = 100;
        while (count < number){
          if(count % 3){
            count = count + 1;
          } else {
            count = count + 1;
          }
          System.Console.WriteLine(count);
        }
      }
      }""")

    println ("=" * 80)
  //LBL09 is empty because thT would be where for loop would be except I haven't made forloop parser
  }

}
