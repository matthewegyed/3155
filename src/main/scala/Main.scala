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
  
  case Plus(e1, e2) =>
      (eval(e1), eval(e2)) match{
        case (Right(ManyVals(vs)), Right(v))=>
          val r= vs.map{vi=>
            eval(Plus(vi, v)) match{
              case Right(vr)=>vr
              case Left(_)=>return Left("ERROR")
            }
          }
          Right(ManyVals(r))
        case (Right(VeryHappy()), _)=>Right(VeryHappy())
        case (_, Right(VeryHappy()))=>Right(VeryHappy())
        case (Right(Cry()), Right(v2))=>Right(v2)
        case (Right(v1), Right(Cry())) if v1.isInstanceOf[Happy]||v1.isInstanceOf[Stun]=>Right(Cry())
        case (Right(v1), Right(v2)) if !v2.isInstanceOf[VeryHappy]&&!v2.isInstanceOf[Cry]=>Right(v1)
        case _ => Left("ERROR")
      }
  case Not(e) => ???
  
}


