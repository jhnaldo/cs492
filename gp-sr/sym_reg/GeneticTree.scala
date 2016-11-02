package sym_reg

import scala.collection.immutable.HashSet

case class GeneticTree(dataList: List[Data]) {
  def training(initial: Option[TreeExpr] = None): TreeExpr = {
    var population: Set[TreeExpr] = initial match {
      case Some(expr) => nextGeneration(expr, randomExpr(MAX_DEPTH), HashSet(expr))
      case None => createPopulation(P_SIZE)
    }
    var k = 0
    var best = TreeExpr(Const(0.0))
    while (k < CONVERGE_ITER) {
      val sorted = sort(population.toList)
      if (sorted.head == best) k += 1
      else {
        best = sorted.head
        k = 0
      }
      println(best.mse(dataList))
      println(best)
      val (first: TreeExpr, second: TreeExpr) = select(sorted)
      val survivals = sorted.slice(0, S_SIZE).toSet
      population = nextGeneration(first, second, survivals)
    }
    best
  }

  def createPopulation(size: Int): Set[TreeExpr] = {
    var set = HashSet[TreeExpr]()
    while (set.size < size) {
      set += randomExpr(MAX_DEPTH)
    }
    set
  }

  def randomExpr(depth: Int): TreeExpr = {
    var expr: TreeExpr = null
    do {
      expr = TreeExpr(randomNode(depth))
    } while (expr.mse(dataList).isNaN)
    expr
  }

  def randomNode(depth: Int): Node = {
    if (depth <= 1 || pass(TERMINAL_RATIO)) {
      uniform(
        Var(uniformIdx(DIMENSION) + 1),
        Const(uniformConst)
      )
    } else if (pass(0.5)) {
      Unary(uniformUnOp, randomNode(depth - 1))
    } else {
      Binary(uniformBinOp, randomNode(depth - 1), randomNode(depth - 1))
    }
  }

  def select(population: List[TreeExpr]): (TreeExpr, TreeExpr) = {
    var lst = List[TreeExpr]()
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

  def sort(population: List[TreeExpr]): List[TreeExpr] = population.sortBy(_.mse(dataList))

  def nextGeneration(father: TreeExpr, mother: TreeExpr, survivals: Set[TreeExpr]): Set[TreeExpr] = {
    var set = survivals
    while (set.size < P_SIZE) {
      val child = crossover(father, mother)
      val mutated = mutate(child)
      set += mutated
    }
    set
  }

  def crossover(father: TreeExpr, mother: TreeExpr): TreeExpr = {
    var expr: TreeExpr = null
    var k = 0
    do {
      expr = TreeExpr(crossover(father.node, mother.node))
      k += 1
    } while (k < MAX_CO_TRIAL && expr.mse(dataList).isNaN)
    if (expr.mse(dataList).isNaN) randomExpr(MAX_DEPTH)
    else expr
  }
  def crossover(left: Node, right: Node): Node = (left, right) match {
    case (Binary(lop, ll, lr), Binary(rop, rl, rr)) =>
      Binary(uniform(lop, rop), crossover(ll, rl), crossover(lr, rr))
    case (Unary(lop, l), Unary(rop, r)) =>
      Unary(uniform(lop, rop), crossover(l, r))
    case _ => uniform(left, right)
  }

  def mutate(expr: TreeExpr): TreeExpr = {
    var newExpr: TreeExpr = null
    var k = 0
    do {
      newExpr = TreeExpr(mutate(expr.node))
      k += 1
    } while (k < MAX_MU_TRIAL && (newExpr == expr || newExpr.mse(dataList).isNaN))
    if (newExpr == expr || newExpr.mse(dataList).isNaN) randomExpr(MAX_DEPTH)
    else newExpr
  }
  def mutate(node: Node): Node = {
    if (pass(MUTATION_RATIO)) pointMutate(node)
    else subtreeMutate(node, MAX_DEPTH)
  }
  def pointMutate(node: Node): Node = node match {
    case Const(value) =>
      Const(pass(POINT_MU_PROB, uniformConst, value))
    case Var(idx) =>
      Var(pass(POINT_MU_PROB, uniformIdx(DIMENSION) + 1, idx))
    case Unary(uop, child) =>
      Unary(pass(POINT_MU_PROB, uniformUnOp, uop), pointMutate(child))
    case Binary(bop, left, right) =>
      Binary(pass(POINT_MU_PROB, uniformBinOp, bop), pointMutate(left), pointMutate(right))
  }
  def subtreeMutate(node: Node, depth: Int): Node = {
    if (pass(1.0 / node.size)) {
      randomNode(depth)
    } else node match {
      case Unary(uop, child) =>
        Unary(uop, subtreeMutate(child, depth-1))
      case Binary(bop, left, right) =>
        if (pass(left.size.toDouble / (left.size + right.size))) {
          Binary(bop, subtreeMutate(left, depth-1), right)
        } else {
          Binary(bop, left, subtreeMutate(right, depth-1))
        }
      case _ => randomNode(depth)
    }
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
  def uniformUnOp: UnOp = uniform(
    // Neg, Abs, Sin, Cos, Tan, ASin, ACos, ATan, SinH, CosH, TanH, Exp, Sqrt, Log
    Neg, Abs, Sin, Cos, Tan, SinH, CosH, TanH, Exp
  )
  def uniformBinOp: BinOp = uniform(
    Add, Sub, Mul, Div, Pow
  )
}
