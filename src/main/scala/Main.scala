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

      case (Right(ManyVals(vs)), Right(v)) =>
        val r = vs.map { vi =>
          eval(Plus(ValueExpr(vi), ValueExpr(v))) match {
            case Right(vr) => vr
            case Left(_) => return Left("ERROR")
          }
        }
        Right(ManyVals(r))

      case (Right(VeryHappy), _) => Right(VeryHappy)
      case (_, Right(VeryHappy)) => Right(VeryHappy)
      case (Right(Cry), Right(v2)) => Right(v2)
      case (Right(v1), Right(Cry)) if v1.isInstanceOf[Happy.type] || v1.isInstanceOf[Stun.type] => Right(Cry)
      case (Right(v1), Right(v2)) if !v2.isInstanceOf[VeryHappy.type] && !v2.isInstanceOf[Cry.type] => Right(v1)
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


