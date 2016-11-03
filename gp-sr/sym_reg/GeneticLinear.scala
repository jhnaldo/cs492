package sym_reg

import scala.collection.immutable.HashSet

case class GeneticLinear(dataList: List[Data]) {
  def training: LinearExpr = {
    var population: List[LinearExpr] = sort(createPopulation(LP_SIZE))
    var best = LinearExpr()
    (1 to LINEAR_ITER).foreach(k => {
      best = population.head
      println(population(0).mse(dataList))
      val (first: LinearExpr, second: LinearExpr) = select(population)
      val survivals = population.slice(0, LS_SIZE).toSet
      population = sort(nextGeneration(first, second, survivals))
    })
    println(best)
    best
  }

  def createPopulation(size: Int): Set[LinearExpr] = {
    var set = HashSet[LinearExpr](LinearExpr(0.0), LinearExpr(100.0), LinearExpr(-100.0))
    while (set.size < size) {
      set += randomExpr
    }
    set
  }

  def randomExpr: LinearExpr = {
    val constant = uniformConst
    val coeffs: XMap = (1 to DIMENSION).foldLeft(XMapEmpty) {
      case (map, idx) => map + (idx -> uniformConst)
    }
    LinearExpr(constant, coeffs)
  }

  def select(population: List[LinearExpr]): (LinearExpr, LinearExpr) = {
    var lst = List[LinearExpr]()
    var k = 0
    while (lst.length < 2) {
      if (pass(SELECT_PROB)) {
        lst ::= population(k)
      }
      k =
        if (k == population.length - 1) 0
        else k + 1
    }
    (lst.head, lst.tail.head)
  }

  def sort(population: Set[LinearExpr]): List[LinearExpr] = population.toList.sortBy(_.mse(dataList))

  def nextGeneration(father: LinearExpr, mother: LinearExpr, survivals: Set[LinearExpr]): Set[LinearExpr] = {
    var set = survivals
    while (set.size < LP_SIZE) {
      val child = crossover(father, mother)
      val mutated = mutate(child)
      set += mutated
    }
    set
  }

  def crossover(father: LinearExpr, mother: LinearExpr): LinearExpr = {
    val constant = crossover(father.constant, mother.constant)
    val coeffs = father.coeff.foldLeft(XMapEmpty) {
      case (map, (idx, c)) => map + (idx -> crossover(c, mother.coeff(idx)))
    }
    LinearExpr(constant, coeffs)
  }
  def crossover(left: Double, right: Double): Double = {
    if (pass(AVERAGE_RATIO))  left + right
    else if (pass(0.5))       left
    else                      right
  }

  def mutate(expr: LinearExpr): LinearExpr = {
    val constant = mutate(expr.constant)
    val coeffs = expr.coeff.foldLeft(XMapEmpty) {
      case (map, (idx, c)) => map + (idx -> mutate(c))
    }
    LinearExpr(constant, coeffs)
  }
  def mutate(value: Double): Double = {
    pass(COEFF_MU_RATIO, uniformConst, value)
  }

  // helpers for probabilties
  def pass(ratio: Double): Boolean = math.random < ratio
  def pass[T](ratio: Double, thenV: T, elseV: T): T = {
    if (pass(ratio)) thenV
    else elseV
  }
  def uniform[T](seq: T*): T = seq(uniformIdx(seq.length))
  def uniformIdx(max: Int): Int = (math.random * max).toInt
  def uniformConst: Double = math.random * CONST_BOUND * (if (pass(0.5)) 1 else -1)
}
