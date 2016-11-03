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

  def toTree: TreeExpr = {
    TreeExpr(Binary(Add, Const(constant), toTree(1, 57)))
  }
  def toTree(from: Int, to: Int): Node = {
    if (from == to) {
      Binary(Mul, Var(from), Const(coeff(from)))
    } else {
      val mid = (from + to + 1) / 2
      Binary(Add, toTree(from, mid-1), toTree(mid, to))
    }
  }

  override def toString: String = toTree.toString
}
