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
    val variable : Parser[Expr] = ident.map(s => Var(List(s)))

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
        case (nm, None)      => Var (nm)
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
  case class Var (nms : List[String])                                 extends Expr
  case class NVar(s: String)                                          extends Expr
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

  def test (p : Parser[Any], s : String) : Unit = {
    val result : fastparse.core.Parsed[Any, Char, String] = p.parse (s) 
    result match {
      case Parsed.Success (value, successIndex) => {
        println ("Successfully parsed \"%s\".  Result is %s.  Index is %d.".format (s, value, successIndex))
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
      case NVar (x)            => getSto (store, x)
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

  

  def main (args : Array[String]) {
    println ("=" * 80)

    val p01 : Parser[Any] = MyParsers.clazz

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

    val p02 : Parser[Any] = MyParsers.clazz

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

    val p03 : Parser[Any] = MyParsers.clazz

    test (p03, """public class Program {
      static void Main(){
        int count = 0;
        int number = 100;
        while (count < number){
          if(count % 3 == 0 && count % 5 == 0){
            count = (count + 1);
          }
          if (count % 3 == 0){
            count = (count + 1);
          }
          if (count % 5 == 0) {
            count = (count + 1);
          }
          System.Console.WriteLine(count);
          count = (count + 1);

        }
      }
      }""")

    println ("=" * 80)


    val p04 : Parser[Any] = MyParsers.statement

    test (p04, """int count = 0;""")

    println ("=" * 80)
  }

}
