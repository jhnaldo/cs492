import sym_reg._

object Training {
  def main(args: Array[String]): Unit = args match {
    case Array(filename) => {
      val dataList = Parser.getData(filename)

      // first phase
      val initial = GeneticLinear(dataList).training.toTree

      // second phase
      val result = GeneticTree(dataList).training(Some(initial))

      println(result.mse(dataList))
      println(result)
    }
    case _ => Console.err.println("we need 2 inputs")
  }
}
