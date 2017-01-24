package csp.ch02

// Programming language concepts for software developers, 2012-02-17
//
// Evaluation, checking, and compilation of object language expressions
// Stack machines for expression evaluation                            
//
// Object language expressions with variable bindings and nested scope 
//
// Original F# version by Peter Sestoft.
// Scala translation by Corin Pitcher 2017-01-06.

object Intcomp1 {

  sealed trait Expr
  case class CstI (n : Int)                           extends Expr
  case class Var (nm : String)                        extends Expr
  case class Let (nm : String, e1 : Expr, e2 : Expr)  extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr) extends Expr
  //HW STUFF
  val fvex1 = 
  Let("x",
  		Prim("+", var("y"),CstI(1)),
  			Prim("+", var("x"), CstI(1)));
  val fvex2 = 
  Let("x",
  		Prim("+" var("x"), CstI(1)),
  			Prim("+", var("x"), CstI(1)));
  val fvex3 = 
  Let("x",
  		(Let"x",
  				Prim("+", var("y"), CstI(1)),
  					Prim("+", var("x"), CstI(1)),
  						Prim("+", var("x"), CstI(1))));
  val fvex4 = 
  Let("x"
  		Let"y",
  			(CstI(1))
  				Prim("+" var("y"), CstI(1)),
  					Prim("+" var("x"), var("y")));

  // Some closed expressions:

  val e1 : Expr = Let ("z", CstI (17), Prim ("+", Var ("z"), Var ("z")))

  val e2 : Expr = Let ("z", CstI (17), 
                       Prim("+", Let ("z", CstI (22), Prim ("*", CstI (100), Var ("z"))),
                            Var ("z")))

  val e3 : Expr = Let ("z", Prim ("-", CstI (5), CstI (4)), 
                       Prim ("*", CstI (100), Var ("z")))

  val e4 : Expr = Prim ("+", Prim ("+", CstI (20), Let ("z", CstI (17), 
                                                        Prim ("+", Var ("z"), CstI (2)))),
                        CstI (30))

  val e5 : Expr = Prim ("*", CstI (2), Let ("x", CstI (3), Prim ("+", Var ("x"), CstI (4))))

  // Evaluation of expressions with variables and bindings

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

  def run (e : Expr) = eval (e, Nil)

  // ----------------------------------------------------------------------

  // Closedness

  def mem [X] (x : X, vs : List[X]) : Boolean = vs.contains (x)

  // Checking whether an expression is closed.  The vs is 
  // a list of the bound variables. 

  def closedin (e : Expr, vs : List[String]) : Boolean = {
    e match {
      case CstI (i) => true
      case Var (x)  => vs.contains (x)
      case Let (x, erhs, ebody) => {
        val vs1 : List[String] = x :: vs 
        closedin (erhs, vs) && closedin (ebody, vs1)
      }
      case Prim (ope, e1, e2) => closedin (e1, vs) && closedin (e2, vs)
    }
  }

  // An expression is closed if it is closed in the empty environment

  def closed1 (e : Expr) = closedin (e, Nil)

  // ----------------------------------------------------------------------

  // Substitution of expressions for variables

  // This version of lookup returns a Var(x) expression if there is no
  // pair (x,e) in the list env --- instead of failing with exception:

  def lookOrSelf (env : List[(String, Expr)], x : String) : Expr = {
    env match {
      case Nil       => Var (x)
      case (y, e)::r => if (x == y) e else lookOrSelf (r, x)
    }
  }

  // Remove (x, _) from env:

  def remove (env : List[(String, Expr)], x : String) : List[(String, Expr)] = {
    env match {
      case Nil        => Nil
      case (y, e):: r => if (x == y) r else (y, e) :: remove (r, x)
    }
  }

  // Naive substitution, may capture free variables:

  def nsubst (e : Expr, env : List[(String, Expr)]) : Expr = {
    e match {
      case CstI (i)           => e
      case Var (x)            => lookOrSelf (env, x)
      case Let (x, erhs, ebody) => {
        val newenv : List[(String, Expr)] = remove (env, x)
        Let (x, nsubst (erhs, env), nsubst (ebody, newenv))
      }
      case Prim (ope, e1, e2) => Prim (ope, nsubst (e1, env), nsubst (e2, env))
    }
  }

  // Some expressions with free variables:

  val e6 : Expr = Prim ("+", Var ("y"), Var ("z"))

  val e6s1 : Expr = nsubst (e6, List ( ("z", CstI (17)) ))
  
  val e6s2 : Expr = nsubst (e6, List ( ("z", Prim ("-", CstI (5), CstI (4))) ))

  val e6s3 : Expr = nsubst (e6, List ( ("z", Prim ("+", Var ("z"), Var ("z"))) ))

  val e7 : Expr = Prim ("+", Let ("z", CstI (22), Prim ("*", CstI (5), Var ("z"))),
                        Var ("z"))

  val e7s1 : Expr = nsubst (e7, List ( ("z", CstI (100)) ))

  // Shows that only the z in the Let rhs gets substituted

  val e8 : Expr = Let ("z", Prim ("*", CstI (22), Var ("z")), Prim ("*", CstI (5), Var ("z")))

  val e8s1 : Expr = nsubst (e8, List ( ("z", CstI (100)) ))

  // Shows (wrong) capture of free variable z under the let:

  val e9 : Expr = Let ("z", CstI (22), Prim ("*", Var ("y"), Var ("z")))

  val e9s1 : Expr = nsubst (e9, List ( ("y", Var ("z")) ))

  val e9s2 : Expr = nsubst (e9, List ( ("z", Prim ("-", CstI (5), CstI (4))) ))

  val newVar : String => String = {
    var n : Int = 0
    def varMaker (x : String) : String = {
      n = 1 + n
      x + n
    }
    varMaker
  }

  // Correct, capture-avoiding substitution

  def subst (e : Expr, env : List[(String, Expr)]) : Expr = {
    e match {
      case CstI (i) => e
      case Var (x)  => lookOrSelf (env, x)
      case Let (x, erhs, ebody) => {
        val newx : String = newVar (x)
        val newenv : List[(String, Expr)] = (x, Var (newx)) :: remove (env, x)
        Let (newx, subst (erhs, env), subst (ebody, newenv))
      }
      case Prim (ope, e1, e2) => Prim (ope, subst (e1, env), subst (e2, env))
    }
  }

  val vale6s1a : Expr = subst (e6, List ( ("z", CstI (17)) ))

  val e6s2a : Expr = subst (e6, List ( ("z", Prim ("-", CstI (5), CstI (4))) ))

  val e6s3a : Expr = subst (e6, List ( ("z", Prim ("+", Var ("z"), Var ("z"))) ))

  // Shows renaming of bound variable z (to z1)

  val e7s1a : Expr = subst (e7, List ( ("z", CstI (100)) ))

  // Shows renaming of bound variable z (to z2)

  val e8s1a : Expr = subst (e8, List ( ("z", CstI (100)) ))

  // Shows renaming of bound variable z (to z3), avoiding capture of free z

  val e9s1a : Expr = subst (e9, List ( ("y", Var ("z")) ))

  // ----------------------------------------------------------------------

  // Free variables

  // Operations on sets, represented as lists.  Simple but inefficient;
  // one could use binary trees, hashtables or splaytrees for
  // efficiency.

  // union (xs, ys) is the set of all elements in xs or ys, without duplicates

  def union [A] (xs : List[A], ys : List[A]) : List[A] = {
    xs match {
      case Nil   => ys
      case x::xr => if (mem (x, ys)) {
        union (xr, ys) 
      } else {
        x :: union (xr, ys)
      }
    }
  }

  // minus (xs, ys)  is the set of all elements in xs but not in ys

  def minus [A] (xs : List[A], ys : List[A]) : List[A] = {
    xs match {
      case Nil   => Nil
      case x::xr => if (mem (x, ys)) minus (xr, ys) else x :: minus (xr, ys)
    }
  }

  // Find all variables that occur free in expression e

  def freevars (e : Expr) : List[String] = {
    e match {
      case CstI (i) => Nil
      case Var (x) => List (x)
      case Let (x, erhs, ebody) => union (freevars (erhs), minus (freevars (ebody), List (x)))
      case Prim (ope, e1, e2) => union (freevars (e1), freevars (e2))
    }
  }
  
  // Alternative definition of closed
  // let closed2 e = (freevars e = []);;

  def closed2 (e : Expr) : Boolean = freevars (e) == Nil

  // ----------------------------------------------------------------------

  // Compilation to target expressions with numerical indexes instead of
  // symbolic variable names.

  sealed trait TExpr                                                   // target expressions
  case class TCstI (n : Int)                             extends TExpr
  case class TVar (n : Int)                              extends TExpr // index into runtime environment
  case class TLet (e1 : TExpr, e2 : TExpr)               extends TExpr // erhs and ebody
  case class TPrim (nm : String, e1 : TExpr, e2 : TExpr) extends TExpr

  // Map variable name to variable index at compile-time

  def getindex [X] (vs : List[X], x : X) : Int = {
    vs match {
      case Nil   => throw new RuntimeException ("Variable not found")
      case y::yr => if (x == y) 0 else 1 + getindex (yr, x)
    }
  }

  // Compiling from expr to texpr

  def tcomp (e : Expr, cenv : List[String]) : TExpr = {
    e match {
      case CstI (i)             => TCstI (i)
      case Var (x)              => TVar (getindex (cenv, x))
      case Let (x, erhs, ebody) => 
        val cenv1 = x :: cenv 
        TLet (tcomp (erhs, cenv), tcomp (ebody, cenv1))
      case Prim (ope, e1, e2)   => TPrim (ope, tcomp (e1, cenv), tcomp (e2, cenv));;
    }
  }

  // Evaluation of target expressions with variable indexes.  The
  // run-time environment renv is a list of variable values (ints).

  def teval (e : TExpr, renv : List[Int]) : Int = {
      e match {
        case TCstI (i) => i
        case TVar (n)  => renv (n)
        case TLet (erhs, ebody) => 
          val xval = teval (erhs, renv)
          val renv1 = xval :: renv 
          teval (ebody, renv1)
        case TPrim ("+", e1, e2) => teval (e1, renv) + teval (e2, renv)
        case TPrim ("*", e1, e2) => teval (e1, renv) * teval (e2, renv)
        case TPrim ("-", e1, e2) => teval (e1, renv) - teval (e2, renv)
        case TPrim (  _,  _,  _) => throw new RuntimeException ("Unknown primitive")
      }
  }

  // Correctness: eval e []  equals  teval (tcomp e []) []

  // ---------------------------------------------------------------------

  // Stack machines

  // Stack machine instructions.  An expressions in postfix or reverse
  // Polish form is a list of stack machine instructions.

  sealed trait RInstr
  case class RCstI (n : Int) extends RInstr
  case object RAdd           extends RInstr
  case object RSub           extends RInstr
  case object RMul           extends RInstr
  case object RDup           extends RInstr
  case object RSwap          extends RInstr

  // A simple stack machine for evaluation of variable-free expressions
  // in postfix form

  def reval (inss : List[RInstr], stack : List[Int]) : Int = {
    (inss, stack) match {
      case (Nil, v :: _)                         => v
      case (Nil, Nil)                            => throw new RuntimeException ("reval: no result on stack!")
      case (RCstI (i) :: insr,             stk)  => reval (insr, (i::stk))
      case (RAdd      :: insr, i2 :: i1 :: stkr) => reval (insr, ((i1+i2)::stkr))
      case (RSub      :: insr, i2 :: i1 :: stkr) => reval (insr, ((i1-i2)::stkr))
      case (RMul      :: insr, i2 :: i1 :: stkr) => reval (insr, ((i1*i2)::stkr))
      case (RDup      :: insr,       i1 :: stkr) => reval (insr, (i1 :: i1 :: stkr))
      case (RSwap     :: insr, i2 :: i1 :: stkr) => reval (insr, (i1 :: i2 :: stkr))
      case _                                     => throw new RuntimeException ("reval: too few operands on stack")
    }
  }

  val rpn1 : Int = reval (List (RCstI (10), RCstI (17), RDup, RMul, RAdd), List ())

  // Compilation of a variable-free expression to a rinstr list

  def rcomp (e : Expr) : List[RInstr] = {
    e match {
      case CstI (i)           => List (RCstI (i))
      case Var (_)            => throw new RuntimeException ("rcomp cannot compile Var")
      case Let (_, _, _)      => throw new RuntimeException ("rcomp cannot compile Let")
      case Prim ("+", e1, e2) => rcomp (e1) ::: rcomp (e2) ::: List (RAdd)
      case Prim ("*", e1, e2) => rcomp (e1) ::: rcomp (e2) ::: List (RMul)
      case Prim ("-", e1, e2) => rcomp (e1) ::: rcomp (e2) ::: List (RSub)
      case Prim (_, _, _)     => throw new RuntimeException ("unknown primitive")
    }
  }

  // Correctness: eval e []  equals  reval (rcomp e) []


  // Storing intermediate results and variable bindings in the same stack

  sealed trait SInstr
  case class SCstI (n : Int) extends SInstr // push integer           
  case class SVar (n : Int)  extends SInstr // push variable from env 
  case object SAdd           extends SInstr // pop args, push sum     
  case object SSub           extends SInstr // pop args, push diff.   
  case object SMul           extends SInstr // pop args, push product 
  case object SPop           extends SInstr // pop value/unbind var   
  case object SSwap          extends SInstr // exchange top and next  

  def seval (inss : List[SInstr], stack : List[Int]) : Int = {
    (inss, stack) match {
      case (Nil, v :: _)                     => v
      case (Nil, Nil)                        => throw new RuntimeException ("seval: no result on stack")
      case (SCstI (i) :: insr,          stk) => seval (insr, (i :: stk))
      case (SVar (i)  :: insr,          stk) => seval (insr, (stk (i) :: stk))
      case (SAdd      :: insr, i2::i1::stkr) => seval (insr, (i1+i2 :: stkr))
      case (SSub      :: insr, i2::i1::stkr) => seval (insr, (i1-i2 :: stkr))
      case (SMul      :: insr, i2::i1::stkr) => seval (insr, (i1*i2 :: stkr))
      case (SPop      :: insr,    _ :: stkr) => seval (insr, stkr)
      case (SSwap     :: insr, i2::i1::stkr) => seval (insr, (i1::i2::stkr))
      case _                                 => throw new RuntimeException ("seval: too few operands on stack")
    }
  }

  // A compile-time variable environment representing the state of
  // the run-time stack.

  sealed trait StackValue
  case object Value             extends StackValue // A computed value
  case class Bound (s : String) extends StackValue // A bound variable
   
  // Compilation to a list of instructions for a unified-stack machine 

  def scomp (e : Expr, cenv : List[StackValue]) : List[SInstr] = {
    e match {
      case CstI (i) => List (SCstI (i))
      case Var (x)  => List (SVar (getindex (cenv, (Bound (x)))))
      case Let (x, erhs, ebody) => 
        scomp (erhs, cenv) ::: scomp (ebody, (Bound (x) :: cenv)) ::: List (SSwap, SPop)
      case Prim ("+", e1, e2) => 
        scomp (e1, cenv) ::: scomp (e2, (Value :: cenv)) ::: List (SAdd)
      case Prim ("-", e1, e2) => 
        scomp (e1, cenv) ::: scomp (e2, (Value :: cenv)) ::: List (SSub)
      case Prim ("*", e1, e2) => 
        scomp (e1, cenv) ::: scomp (e2, (Value :: cenv)) ::: List (SMul)
      case Prim (_, _, _) => throw new RuntimeException ("scomp: unknown operator")
      case _ => Nil
    }
  }

  val s1 : List[SInstr] = scomp (e1, Nil)
  val s2 : List[SInstr] = scomp (e2, Nil)
  val s3 : List[SInstr] = scomp (e3, Nil)
  val s5 : List[SInstr] = scomp (e5, Nil)

  // Output the integers in list inss to the text file called fname:

  def intsToFile (inss : List[Int], fname : String) = {
    val text : StringBuilder = inss.addString (new StringBuilder, " ")
    println (fname + text)
  }

}


