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
    val stringLiteral : Parser[String] = P ("\"".? ~ (alpha ~ (alpha | CharIn ('0' to '9') | " ").rep (0)).! ~ "\"".?)
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
    val addSub : Parser[Expr] = P (mult ~ (("+" | "-").! ~ mult).rep.map (s => s.toList)).map (foldAssocLeft)
   
   //PARSERS! <3
     
     val assigStatement : P[Statement] = 
      P (( "int".? ~ ident ~ "=" ~ integer ~ ";").map{case(nm, e1) => AssigStatement(nm, e1)})

     val printStatement : P [Statement] =
      P (("System.Console.WriteLine" ~ "(" ~ (stringLiteral | ident) ~ ")" ~ ";").map{case(e) => Print(e)})
    
     val statements : P [List[Statement]] =
       P (((assigStatement|printStatement).rep).map { case (s) => s.toList})//.map{case(nm, e1) => AssigStatement(nm, e1)}) 

     val methods : P[Methods] =
      P ("public".? ~ "static".? ~ ident.map( s => ()) ~ ident  ~ "(" ~ (ident ~ ident ~ ",".?).rep ~ ")" ~ "{" ~ statements ~ "}").map{case(nm, parameters, body) => Methods(nm, parameters.toList, body)}
  }
  case class Methods(nm: String, parameters: List[(String, String)], body : List[Statement])
  //case class Clazz (nm: String, ss: Statements, m: Methods, )

  sealed trait Expr
  case class CstI (n : Int)                           extends Expr
  case class Var (nm : String)                        extends Expr
  case class Let (nm : String, e1 : Expr, e2 : Expr)  extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr) extends Expr

  sealed trait Statement
  case class Print(s: String)                                                    extends Statement
  case class PrintStatement(e: Expr)                                             extends Statement
  case class AssigStatement(nm: String, e1: Expr)                                extends Statement 
  case class If (e : Expr, s1 : Statement, s2 : Statement)                       extends Statement
  case class Block (ss : List[Statement])                                        extends Statement
  case class For (nm : String, low : Expr, high : Expr, s : Statement)           extends Statement
  case class While (e : Expr, s : Statement)                                     extends Statement

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

  def exec (s : Statement, store : NaiveStore) : NaiveStore = {
    s match {
      case AssigStatement (nm, e)            => {
        val v : Int = eval (e, store)
        // println ("store is %s".format (store))
        // println ("assigning %d to %s".format (v, nm))
        setSto (store, nm, v)
      }
      case If (e, s1, s2)          => exec (if (eval (e, store) != 0) s1 else s2, store)
      case Block (ss)              => {
        def loop (ss2 : List[Statement], store2 : NaiveStore) : NaiveStore = {
          ss2 match {
            case Nil       => store2
            case s2 :: ss3 => loop (ss3, exec (s2, store2))
          }
        }
        loop (ss, store)
      }
      case For (nm, low, high, s)  => {
        val start : Int = eval (low, store) 
        val stop : Int = eval (high, store)
        def loop (i : Int, sto : NaiveStore) : NaiveStore = {
          if (i > stop) {
            sto 
          } else {
            loop (i + 1, exec (s, setSto (sto, nm, i)))
          }
        }
        loop (start, store)
      }
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
      case Print (s)               => {
        println ((s, store))
        store
      }

      case PrintStatement (e)               => {
        println (eval (e, store))
        store
      }
    }
  }


 val helloWorld : Statement = {
    Block (
      List (
        Print("hello world")
      )
    )
  }


  val factorial : Statement = {
      Block (
        List (
          AssigStatement (
            "fact", 
            CstI (1)
          ),
          For (
            "i", 
            CstI (1), 
            CstI (10), 
            AssigStatement (
              "fact", 
              Prim ("*", Var ("fact"), Var ("i"))
            )
          ),
          PrintStatement (Var ("fact"))
        )
      )
    }

  def main (args : Array[String]) {
    println ("=" * 80)

    val p01 : Parser[Any] = MyParsers.methods
    //test (p01, "int x = 2;")
    //test (p01, "System.Console.WriteLine ( x );")
    test (p01, """public static void main(){
      System.Console.WriteLine("hello world");
    }""")

    println ("=" * 80)

    test (p01, """static int Factorial(int number)
    {
        int accumulator = 1;
        for (int factor = 1; factor <= number; factor++)
        {
            accumulator *= factor;
        }
        return accumulator;
    }
 
    static void Main()
    {
        System.Console.WriteLine(Factorial(10));
    }""")

    println ("=" * 80)

    // TODO: dafuq be NaiveStore
    exec(factorial, emptyStore)
    println ("=" * 80)

    exec(helloWorld, emptyStore)
    println ("=" * 80)


  }
}
