package csp.ch01


object Expressions {
  
  sealed trait Expr
  case class CstI (n:Int)                       extends Expr
  case class Prim (op:String, e1:Expr, e2:Expr) extends Expr

  def eval (e:Expr) : Int = {
    e match {
      case CstI (n)           => n
      case Prim ("+", e1, e2) => eval (e1) + eval (e2)
      case Prim ("*", e1, e2) => eval (e1) * eval (e2)
      case Prim ("-", e1, e2) => eval (e1) - eval (e2)
      case Prim (  _,  _,  _) => throw new RuntimeException ("unknown primitive")
    }
  }

  def evalm (e:Expr) : Int = {
    e match {
      case CstI (n)           => n
      case Prim ("+", e1, e2) => evalm (e1) + evalm (e2)
      case Prim ("*", e1, e2) => evalm (e1) * evalm (e2)
      case Prim ("-", e1, e2) => {
        val res = evalm (e1) - evalm (e2)
        if (res < 0) 0 else res
      }
      case Prim (  _,  _,  _) => throw new RuntimeException ("unknown primitive")
    }
  }

}
