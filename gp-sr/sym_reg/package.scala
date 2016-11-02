package object sym_reg {
  type XMap = Map[Int, Double]
  val XMapEmpty: XMap = Map[Int, Double]()
  val XMapZero: XMap = (1 to DIMENSION).foldLeft(XMapEmpty) {
    case (map, idx) => map + (idx -> 0.0)
  }

  // options
  val DIMENSION: Int = 57             // the number of variables
  val MAX_DEPTH: Int = 10             // the maximum depth of tree expression
  val TERMINAL_RATIO: Double = 0.1    // the probaibilty to select terminal node
  val P_SIZE: Int = 100               // the size of populations
  val S_SIZE: Int = 2                 // the number of survivals in parent populations
  val CONST_BOUND: Double = 100.0     // the upper bound for constant values
  val MUTATION_RATIO: Double = 0.3    // the ratio of point/sub-tree mutations
  val MAX_CO_TRIAL: Int = 3           // max trials for each crossover
  val MAX_MU_TRIAL: Int = 3           // max trials for each mutation
  val POINT_MU_PROB: Double = 0.1     // the probability of point mutation for each node
  val SELECT_PROB: Double = 0.5       // the probability of selection
  val CONVERGE_ITER: Int = 100        // the number of same best for convergence
  val LINEAR_ITER: Int = 100          // the number of iterations for linear regression
}
