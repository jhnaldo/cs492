import scala.io.Source

object Test {
  val DIMENSION = 57
  var expr: String = ""
  def main(args: Array[String]): Unit = args match {
    case Array(str, filename) => {
      expr = str
      val result = mse(fromFile(filename))
      println(result)
    }
    case _ => Console.err.println("we need 2 inputs")
  }

  def fromFile(filename: String): List[(List[Double], Double)] = {
    Source
      .fromFile(filename)
      .getLines
      .foldLeft(List[List[String]]()) {
        case (lst, str) => str.split(",").toList :: lst
      }.dropRight(1).foldLeft(List[(List[Double], Double)]()) {
        case (pairList, lst) => {
          val dList = lst.map(_.toDouble)
          (dList.dropRight(1), dList.last) :: pairList
        }
      }
  }

  def mse(inputs: List[(List[Double], Double)]): Double = {
    inputs.foldLeft(0.0) {
      case (sum, (xs, y)) => {
        val diff = eval(xs) - y
        sum + (diff * diff)
      }
    } / inputs.length
  }

  def eval(xs: List[Double]): Double = {
    val stack = expr.split(" ").foldLeft(List[Double]()) {
      case (stack, str) => {
        def unary(op: Double => Double): List[Double] =
          op(stack.head) :: stack.tail
        def binary(op: (Double, Double) => Double): List[Double] = {
          val left = stack.tail.head
          val right = stack.head
          op(left, right) :: stack.tail.tail
        }

        str match {
          case str if str.startsWith("x") => xs(str.substring(1).toInt - 1) :: stack
          case "~" => unary(- _)
          case "abs" => unary(math.abs)
          case "sin" => unary(math.sin)
          case "cos" => unary(math.cos)
          case "tan" => unary(math.tan)
          case "asin" => unary(math.asin)
          case "acos" => unary(math.acos)
          case "atan" => unary(math.atan)
          case "sinh" => unary(math.sinh)
          case "cosh" => unary(math.cosh)
          case "tanh" => unary(math.tanh)
          case "exp" => unary(math.exp)
          case "sqrt" => unary(math.sqrt)
          case "log" => unary(math.log)
          case "+" => binary(_ + _)
          case "-" => binary(_ - _)
          case "*" => binary(_ * _)
          case "/" => binary(_ / _)
          case "^" => binary(math.pow)
          case _ => str.toDouble :: stack
        }
      }
    }
    stack.head
  }
}
