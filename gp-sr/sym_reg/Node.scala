package sym_reg

sealed abstract class Node {
  var size: Int
  var height: Int
  def eval(xs: XMap): Double
}

// Terminal
case class Var(idx: Int) extends Node {
  var size: Int = 1
  var height: Int = 1
  def eval(xs: XMap): Double = xs(idx)
  override def toString: String = s"x$idx"
}
case class Const(value: Double) extends Node {
  var size: Int = 1
  var height: Int = 1
  def eval(xs: XMap): Double = value
  override def toString: String = value.toString
}

// Non-Terminal
case class Unary(op: UnOp, child: Node) extends Node {
  var size: Int = child.size + 1
  var height: Int = child.height + 1
  def eval(xs: XMap): Double = op.func(child.eval(xs))
  override def toString: String = s"$child $op"
}
case class Binary(op: BinOp, left: Node, right: Node) extends Node {
  var size: Int = left.size + right.size + 1
  var height: Int = math.max(left.height, right.height) + 1
  def eval(xs: XMap): Double = op.func(left.eval(xs), right.eval(xs))
  override def toString: String = s"$left $right $op"
}

sealed abstract class UnOp(val symbol: String, val func: Double => Double) {
  override def toString: String = symbol
}
case object Neg extends UnOp("~", - _)
case object Abs extends UnOp("abs", math.abs)
case object Sin extends UnOp("sin", math.sin)
case object Cos extends UnOp("cos", math.cos)
case object Tan extends UnOp("tan", math.tan)
case object ASin extends UnOp("asin", math.asin)
case object ACos extends UnOp("acos", math.acos)
case object ATan extends UnOp("atan", math.atan)
case object SinH extends UnOp("sinh", math.sinh)
case object CosH extends UnOp("cosh", math.cosh)
case object TanH extends UnOp("tanh", math.tanh)
case object Exp extends UnOp("exp", math.exp)
case object Sqrt extends UnOp("sqrt", math.sqrt)
case object Log extends UnOp("log", math.log)

sealed abstract class BinOp(val symbol: String, val func: (Double, Double) => Double) {
  override def toString: String = symbol
}
case object Add extends BinOp("+", _ + _)
case object Sub extends BinOp("-", _ - _)
case object Mul extends BinOp("*", _ * _)
case object Div extends BinOp("/", _ / _)
case object Pow extends BinOp("^", math.pow)
