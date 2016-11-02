import sym_reg._

object Training {
  def main(args: Array[String]): Unit = args match {
    case Array(filename) => {
      val dataList = Parser.getData(filename)
      println(GeneticTree(dataList).training())
    }
    case Array(filename, mode) => {
      val dataList = Parser.getData(filename)

      val result = mode match {
        case "linear_local" => LocalLinear(dataList).training.toTree
        case "linear_gen" => GeneticLinear(dataList).training.toTree
        case "tree_gen" => GeneticTree(dataList).training()
      }

      println(result.mse(dataList))
      println(result)
    }
    case _ => Console.err.println("we need 2 inputs: filename and mode")
  }
}
