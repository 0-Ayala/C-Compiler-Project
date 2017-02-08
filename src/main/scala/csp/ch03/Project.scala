package csp.ch03

// From SBT: ~run-main csp.ch03.Project

object Project {
  object MyParsersNoWhitespace {
    import fastparse.all._

    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)
    val integer : Parser[Expr] = digits.map (n => CstI (n))

    val keywords : List[String] = List ("let", "in", "end", "string")
    val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z')).!)
    val ident : Parser[String] = P ((alpha ~ (alpha | CharIn ('0' to '9')).rep (0)).!).filter (s => !keywords.contains (s))
    val stringAssignment : P[Unit] = P ("string" ~ ident.map (s=>() ) )
  }

  object MyParsers {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace (" ".rep)
    }

    import fastparse.noApi._
    import White._

    import MyParsersNoWhitespace._

    val parens : Parser[Expr] = P (integer | ("(" ~ addSub ~ ")"))
    val mult : Parser[Expr] = P (parens ~ ("*".! ~ parens).rep.map (s => s.toList)).map (foldAssocLeft)
    val addSub : Parser[Expr] = P (mult ~ (("+" | "-").! ~ mult).rep.map (s => s.toList)).map (foldAssocLeft)
    val stringAssignment : P[Unit] = P ("string" ~ ident.map (s=>() ) )

    //val start : Parser[Unit] = P (putYourStuffHere ~ End)
  }

  sealed trait Expr
  case class CstI (n : Int)                           extends Expr
  case class Var (nm : String)                        extends Expr
  case class Let (nm : String, e1 : Expr, e2 : Expr)  extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr) extends Expr

  def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
    p match {
      case (e1, Nil) => e1
      case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
    }
  }

  import fastparse.all.{Parsed,Parser}

  def test (p : Parser[Unit], s : String) : Unit = {
    val result : fastparse.core.Parsed[Unit, Char, String] = p.parse (s) 
    result match {
      case Parsed.Success (value, successIndex) => {
        println ("Successfully parsed \"%s\".  Result is %s.  Index is %d.".format (s, value, successIndex))
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (s, lastParser, index, extra))
      }
    }
  }

  def main (args : Array[String]) {
    println ("=" * 80)

    val p01 : Parser[Unit] = MyParsers.stringAssignment
    test (p01, "string s")
    println ("=" * 80)
  }
}
