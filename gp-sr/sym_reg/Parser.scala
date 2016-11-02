package sym_reg

import scala.io.Source

object Parser {
  def parse(str: String): TreeExpr = TreeExpr({
    val stack = str.split(" ").foldLeft(List[Node]()) {
      case (stack, str) => {
        def unary(op: UnOp): List[Node] =
          Unary(op, stack.head) :: stack.tail
        def binary(op: BinOp): List[Node] = {
          val left = stack.tail.head
          val right = stack.head
          Binary(op, left, right) :: stack.tail.tail
        }
        str match {
          case str if str.startsWith("x") => Var(str.substring(1).toInt) :: stack
          case "~" => unary(Neg)
          case "abs" => unary(Abs)
          case "sin" => unary(Sin)
          case "cos" => unary(Cos)
          case "tan" => unary(Tan)
          case "asin" => unary(ASin)
          case "acos" => unary(ACos)
          case "atan" => unary(ATan)
          case "sinh" => unary(SinH)
          case "cosh" => unary(CosH)
          case "tanh" => unary(TanH)
          case "exp" => unary(Exp)
          case "sqrt" => unary(Sqrt)
          case "log" => unary(Log)
          case "+" => binary(Add)
          case "-" => binary(Sub)
          case "*" => binary(Mul)
          case "/" => binary(Div)
          case "^" => binary(Pow)
          case _ => Const(str.toDouble) :: stack
        }
      }
    }
    stack.head
  })

  def getData(filename: String): List[Data] = Source
    .fromFile(filename)
    .getLines
    .foldLeft(List[List[String]]()) {
      case (lst, str) => str.split(",").toList :: lst
    }.dropRight(1).foldLeft(List[Data]()) {
      case (pairList, lst) => {
        val dList = lst.map(_.toDouble)
        val map = dList.dropRight(1).zipWithIndex.foldLeft(XMapEmpty) {
          case (map, (x, k)) => map + (k+1 -> x)
        }
        Data(map, dList.last) :: pairList
      }
    }
}
