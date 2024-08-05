//**TODO: Your code here**
sealed trait Value
case object Cry extends Value       // T.T
case object Happy extends Value     // :)
case object VeryHappy extends Value // UwU
case object Sleepy extends Value    // -.- zzZ
case object Stun extends Value      // \circ . \circ
case class ManyVals(vals: List[Value]) extends Value // [v*]

sealed trait Expr
case class ValueExpr(v: Value) extends Expr
case class ManyExprs(exprs: List[Expr]) extends Expr // [e*]
case class Plus(e1: Expr, e2: Expr) extends Expr     // e1 + e2
case class Not(e: Expr) extends Expr                 // !e


// Main evaluation function
def eval(expr: Expr): Either[String, Value] = expr match {
  case ValueExpr(v) => Right(v)
  
  case ManyExprs(exprs) => ???

  case Plus(e1, e2) => ???
  
  case Not(e) => ???
  
}


