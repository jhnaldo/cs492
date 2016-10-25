package sym_reg

sealed abstract class Node {
  def eval(xs: XMap): Double
}
case class Var(idx: Int) extends Node {
  def eval(xs: XMap): Double = xs(idx)
}
case class Unary(op: UnOp, child: Node) extends Node {
  def eval(xs: XMap): Double = op.func(child.eval(xs))
}
case class Binary(op: BinOp, left: Node, right: Node) extends Node {
  def eval(xs: XMap): Double = op.func(left.eval(xs), right.eval(xs))
}
case class Const(value: Double) extends Node {
  def eval(xs: XMap): Double = value
}

sealed abstract class UnOp(val func: Double => Double)
case object Neg extends UnOp(- _)
case object Abs extends UnOp(math.abs)
case object Sin extends UnOp(math.sin)
case object Cos extends UnOp(math.cos)
case object Tan extends UnOp(math.tan)
case object ASin extends UnOp(math.asin)
case object ACos extends UnOp(math.acos)
case object ATan extends UnOp(math.atan)
case object SinH extends UnOp(math.sinh)
case object CosH extends UnOp(math.cosh)
case object TanH extends UnOp(math.tanh)
case object Exp extends UnOp(math.exp)
case object Sqrt extends UnOp(math.sqrt)
case object Log extends UnOp(math.log)

sealed abstract class BinOp(val func: (Double, Double) => Double)
case object Add extends BinOp(_ + _)
case object Sub extends BinOp(_ - _)
case object Mul extends BinOp(_ * _)
case object Div extends BinOp(_ / _)
case object Pow extends BinOp(math.pow)
