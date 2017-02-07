package csp.ch03

// From SBT: ~run-main csp.ch03.Ex05

object Ex05 {
  object MyParsersNoWhitespace {
    import fastparse.all._

    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)
    val integer : Parser[Expr] = digits.map (n => CstI (n))

    val keywords : List[String] = List ("let", "in", "end")
    val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z')).!)
    val ident : Parser[String] = P ((alpha ~ (alpha | CharIn ('0' to '9')).rep (0)).!).filter (s => !keywords.contains (s))
    val variable : Parser[Expr] = ident.map (s => Var (s))
  }

  object MyParsers {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace (" ".rep)
    }

    import fastparse.noApi._
    import White._

    import MyParsersNoWhitespace._

    val atom : Parser[Expr] = P (integer | variable)
    val parens : Parser[Expr] = P (atom | ("(" ~ addSub ~ ")") | ("let" ~ ident ~ "=" ~ addSub ~ "in" ~ addSub ~ "end").map { case (x, erhs, ebody) => Let (x, erhs, ebody) })
    val mult : Parser[Expr] = P (parens ~ ("*".! ~ parens).rep.map (s => s.toList)).map (foldAssocLeft)
    val addSub : Parser[Expr] = P (mult ~ (("+" | "-").! ~ mult).rep.map (s => s.toList)).map (foldAssocLeft)
    val start : Parser[Expr] = P (addSub ~ End)
  }

  sealed trait Expr
  case class CstI (n : Int)                           extends Expr
  case class Var (nm : String)                        extends Expr
  case class Let (nm : String, e1 : Expr, e2 : Expr)  extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr) extends Expr

  def lookup (env : List[(String, Int)], x : String) : Int = {
    env match {
      case Nil         => throw new RuntimeException (x + " not found")
      case (y, v) :: r => if (x == y) v else lookup (r, x)
    }
  }

  def eval (e : Expr, env : List[(String, Int)]) : Int = {
    e match {
      case CstI (i)           => i
      case Var (x)            => lookup (env, x)
      case Let (x, erhs, ebody) => {
        val xval : Int = eval (erhs, env)
        val env1 : List[(String, Int)] = (x, xval) :: env 
        eval (ebody, env1)
      }
      case Prim ("+", e1, e2) => eval (e1, env) + eval (e2, env)
      case Prim ("*", e1, e2) => eval (e1, env) * eval (e2, env)
      case Prim ("-", e1, e2) => eval (e1, env) - eval (e2, env)
      case Prim (  _,  _,  _) => throw new RuntimeException ("unknown primitive")
    }
  }

  def pp (e : Expr) : String = {
    e match {
      case CstI (i)             => i.toString
      case Var (x)              => x
      case Let (x, erhs, ebody) => "let %s = %s in %s end".format (x, pp (erhs), pp (ebody))
      case Prim (op, e1, e2)    => "(%s %s %s)".format (pp (e1), op, pp (e2))
    }
  }

  def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
    p match {
      case (e1, Nil) => e1
      case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
    }
  }

  import fastparse.all.{Parsed,Parser}

  def testEval (p : Parser[Expr], s : String) : Unit = {
    val result : fastparse.core.Parsed[Expr, Char, String] = p.parse (s) 
    result match {
      case Parsed.Success (value, successIndex) => {
        println ("Successfully parsed \"%s\".  Result is %s.  Index is %d.".format (s, value, successIndex))
        println ("Pretty print: %s.".format (pp (value)))
        try {
          println ("Evaluates to: %d.".format (eval (value, Nil)))
        } catch {
          case (e:Exception) => println ("Evaluation failed: %s".format (e))
        }
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (s, lastParser, index, extra))
      }
    }
  }

  def main (args : Array[String]) {
    println ("=" * 80)

    val p01 : Parser[Expr] = MyParsers.start
    testEval (p01,  "1 + 2 * 3")
    testEval (p01, "1-2- 3")
    testEval (p01, "1 + -2")
    testEval (p01, "x++")
    testEval (p01, "1 + 1.2")
    testEval (p01,  "1 + ")
    testEval (p01, "let z = (17) in z + 2 * 3 end")
    testEval (p01, "let z = 17) in z + 2 * 3 end")
    println ("=" * 80)
  }
}
