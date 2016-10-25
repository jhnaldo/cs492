import sym_reg._

object Test {
  def main(args: Array[String]): Unit = args match {
    case Array(str, filename) => {
      val expr = Parser.parse(str)
      val dataList = Parser.getData(filename)
      val result = expr.mse(dataList)
      println(result)
    }
    case _ => Console.err.println("we need 2 inputs")
  }
}
