package sym_reg

case class TreeExpr(node: Node) extends Expr {
  private var cache: Option[Double] = None
  def eval(xs: XMap): Double = node.eval(xs)
  override def toString: String = node.toString
}
