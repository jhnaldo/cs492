package sym_reg

case class LinearExpr(
  constant: Double = 0.0,
  coeff: XMap = XMapZero
) extends Expr {
  def eval(xs: XMap): Double = {
    (1 to DIMENSION).foldLeft(constant) {
      case (sum, idx) => {
        sum + (coeff(idx) * xs(idx))
      }
    }
  }

  def toTree: TreeExpr = TreeExpr(Const(0.0)) // TODO
  override def toString: String = {
    var str = constant.toString
    coeff.toSeq
      .sortBy { case (idx, _) => idx }
      .foreach { case (idx, c) =>
        str += s"[$idx:$c]"
      }
    str
  }
}
