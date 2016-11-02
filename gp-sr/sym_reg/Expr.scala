package sym_reg

abstract class Expr {
  private var cache: Option[Double] = None
  def eval(xs: XMap): Double
  def mse(inputs: List[Data]): Double = cache match {
    case Some(value) => value
    case None => {
      val value = inputs.foldLeft(0.0) {
        case (sum, Data(xs, y)) => {
          val diff = eval(xs) - y
          sum + (diff * diff)
        }
      } / inputs.length
      cache = Some(value)
      value
    }
  }
}
