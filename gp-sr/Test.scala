import sym_reg._

object Test {
  def main(args: Array[String]): Unit = args match {
    case Array(str, filename) => {
      val expr = Parser.parse(str)
      val dataList = Parser.getData(filename)
      val result = mse(expr, dataList)
      println(result)
    }
    case _ => Console.err.println("we need 2 inputs")
  }

  def mse(expr: Node, inputs: List[Data]): Double = {
    inputs.foldLeft(0.0) {
      case (sum, Data(xs, y)) => {
        val diff = expr.eval(xs) - y
        sum + (diff * diff)
      }
    } / inputs.length
  }
}
