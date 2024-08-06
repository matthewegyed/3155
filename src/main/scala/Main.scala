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
  
  case ManyExprs(exprs) =>
    exprs.foldLeft[Either[String, List[Value]]](Right(Nil)) {
      case (Right(acc), expr) =>
        eval(expr) match {
          case Right(value) => Right(value :: acc)
          case Left(err) => Left(err)
        }
      case (left @ Left(_), _) => left
    }.map(vals => ManyVals(vals.reverse))

  case Plus(e1, e2) =>
    (eval(e1), eval(e2)) match {
      // Both are lists
      case (Right(ManyVals(vs1)), Right(ManyVals(vs2))) =>
        val minLength = Math.min(vs1.length, vs2.length)
        val combined = (0 until minLength).map { i =>
          (vs1(i), vs2(i)) match {
            case (VeryHappy, _) | (_, VeryHappy) => VeryHappy
            case (Cry, v) => v
            case (v, Cry) => v
            case (Happy, _) | (_, Happy) => Happy
            case (Stun, Stun) => Cry
            case _ => vs1(i)
          }
        }
        val remaining = if (vs1.length > minLength) vs1.drop(minLength) else vs2.drop(minLength)
        Right(ManyVals(combined.toList ++ remaining))

      // Left is a list
      case (Right(ManyVals(vs)), Right(v)) =>
        val result = vs.map { vi =>
          (vi, v) match {
            case (VeryHappy, _) | (_, VeryHappy) => VeryHappy
            case (Cry, v) => v
            case (v, Cry) => v
            case (Happy, _) | (_, Happy)=> Happy
            case (Stun, Stun) => Cry
            case _  => vi
          }
        }
        Right(ManyVals(result))

      // Right is a list
      case (Right(v), Right(ManyVals(vs))) =>
        val result = vs.map { vi =>
          (v, vi) match {
            case (VeryHappy, _) | (_, VeryHappy) => VeryHappy
            case (Cry, v) => v
            case (v, Cry) => v
            case (Happy, _) | (_, Happy) => Happy
            case (Stun, Stun) => Cry
            case _ => v
          }
        }
        Right(ManyVals(result))

      // Both are single values
      case (Right(v1), Right(v2)) =>
        val result = (v1, v2) match {
          case (VeryHappy, _) | (_, VeryHappy) => VeryHappy
          case (Cry, v) => v
          case (v, Cry) => v
          case (Happy, _) | (_, Happy)=> Happy
          case (Stun, Stun)=> Cry
          case _ => v1
        }
        Right(result)

      case _ => Left("ERROR")
    }
  case Not(e) => {
    eval(e) match {
      case Right(Stun) => Right(Sleepy)
      case Right(Sleepy)=> Right(Stun)
      case Right(Happy)=> Right(Cry)
      case Right(VeryHappy)=> Right(Cry)
      case Right(Cry)=> Right(VeryHappy)
      
      case Right(ManyVals(ls)) if (ls.size>1)=> {
        val result = ls.tail.foldLeft[Either[String, Value]](Right(ls.head)){
            case(Right(accumulated),current)=> eval(Plus(ValueExpr(accumulated), ValueExpr(current)))
            case(Left("ERROR"),_) => Left("ERROR")
          }
        result
      }
      case _ => Left("ERROR")
    }
  }
  
}


