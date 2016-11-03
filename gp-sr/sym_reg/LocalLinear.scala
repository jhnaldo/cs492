package sym_reg

import scala.collection.immutable.HashSet

case class LocalLinear(dataList: List[Data]) {
  def training: LinearExpr = {
    val best = (1 to LOCAL_ITER).foldLeft(LinearExpr()) {
      case (expr, k) => {
        val next = uniformIdx(DIMENSION + 1) match {
          case 0 => findBest(expr, findBestConst)
          case idx => findBest(expr, findBestCoeff(idx))
        }
        if (k % 1000 == 0) {
          println(k)
          println(next.mse(dataList))
          println(next)
        }
        next
      }
    }
    println(best)
    best
  }

  def findBest(expr: LinearExpr, helper: (LinearExpr, Double) => LinearExpr): LinearExpr = {
    val (range: Double, best: LinearExpr) =
      (0 until 10).foldLeft((CONST_BOUND / 10, expr)) {
        case ((range, expr), _) =>
          (range / 5, helper(expr, range))
      }
    best
  }

  def findBestConst(expr: LinearExpr, range: Double): LinearExpr = {
    var best = expr
    var keep = true
    do {
      val lower = expr.copy(constant = safe(expr.constant - range))
      val upper = expr.copy(constant = safe(expr.constant + range))
      val bmse = best.mse(dataList)
      val lmse = lower.mse(dataList)
      val umse = upper.mse(dataList)
      if (bmse > lmse) {
        best = lower
      } else if (bmse > umse) {
        best = upper
      } else {
        keep = false
      }
    } while (keep)
    best
  }

  def findBestCoeff(idx: Int)(expr: LinearExpr, range: Double): LinearExpr = {
    var best = expr
    var keep = true
    do {
      val lower = expr.copy(coeff = expr.coeff + (idx -> safe(expr.coeff(idx) - range)))
      val upper = expr.copy(coeff = expr.coeff + (idx -> safe(expr.coeff(idx) + range)))
      val bmse = best.mse(dataList)
      val lmse = lower.mse(dataList)
      val umse = upper.mse(dataList)
      if (bmse > lmse) {
        best = lower
      } else if (bmse > umse) {
        best = upper
      } else {
        keep = false
      }
    } while (keep)
    best
  }

  def safe(x: Double): Double = {
    if (x < -CONST_BOUND) -CONST_BOUND
    else if (x > CONST_BOUND) CONST_BOUND
    else x
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
