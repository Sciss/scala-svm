package me.iamzsx.scala.svm

abstract class QMatrix {
  def swapIndex(i: Int, j: Int)
  def apply(i: Int, j: Int): Double
}

class OneClassQMatrix(val problem: SVMProblem, val param: SVMParameter) extends QMatrix {
  val x: Array[List[SVMNode]] = Array(List(new SVMNode(1, 0.2)))
  val qd = (0 until problem.size).map(i => param.kernel(x(i), x(i)))

  def swapIndex(i: Int, j: Int) {

  }

  override def apply(i: Int, j: Int) = {
    if (i == j) {
      qd(i)
    } else {
      param.kernel(x(i), x(j))
    }
  }
}

class Solver(
  problem: SVMProblem,
  param: SVMParameter,
  Q: QMatrix,
  p: Array[Double],
  y: Array[Int],
  alpha_ : Array[Double],
  Cp: Double,
  Cn: Double) {

  private val epsilon = param.eps
  private val shrinking = param.shrinking

  val LOWER_BOUND = 2
  val UPPER_BOUND = 1
  val FREE = 3

  val TAU = 1e-12

  private val len: Int = problem.size

  private val alpha = alpha_.clone

  var activeSize = 1

  private def getAlphaStatus(i: Int) = {
    if (alpha(i) >= getC(i)) UPPER_BOUND
    else if (alpha(i) < 0) LOWER_BOUND
    else FREE
  }

  private def getC(i: Int) = if (y(i) > 0) Cp else Cn

  private def isUpperBound(i: Int) = getAlphaStatus(i) == UPPER_BOUND
  private def isLowerBound(i: Int) = getAlphaStatus(i) == LOWER_BOUND
  private def isFree(i: Int) = getAlphaStatus(i) == FREE

  var counter = 1;

  def solve: Solution = {
    val G = p
    val G_bar = Array.fill(len)(0)

    optimize

    new Solution(
      calculateObjectiveValue,
      calculateRho,
      Cp,
      Cn,
      0)
  }

  def doShrinking {

  }

  def calculateRho = {
    var ub = Double.MaxValue
    var lb = Double.MinValue
    var sum_free = 0.0
    var nr_free = 0
    for (i <- 0 until activeSize) {
      val yG = y(i) * G(i)
      if (isUpperBound(i)) {
        if (y(i) == -1) ub = ub min yG else lb = lb max yG
      } else if (isLowerBound(i)) {
        if (y(i) == +1) ub = ub min yG else lb = lb max yG
      } else {
        nr_free += 1
        sum_free += yG
      }
    }

    if (nr_free > 0) {
      sum_free / nr_free
    } else {
      (ub + lb) / 2
    }
  }

  def calculateObjectiveValue = {
    {
      for (i <- 0 until len)
        yield alpha(i) * (G(i) + p(i))
    }.sum / 2
  }

  def optimize {
    var iter = 0
    val maxIter = getMaxIteration
    for (iter <- 0 until maxIter) {

      counter = counter - 1
      if (counter == 0) {
        counter = len min 1000
        if (shrinking) {
          doShrinking
        }
        println(".")
      }

      // TODO 
      val (i, j) = selectWorkingSet
      if (!isValidWorkingSet(i, j)) {
        reconstructGradient

        activeSize = len
        // TODO

        val (i, j) = selectWorkingSet
        if (!isValidWorkingSet(i, j)) {
          return
        } else {
          counter = 1;
        }
      }

      var quadCoef = Q(i, i) + Q(j, j) - 2 * y(i) * y(j) * Q(i, j)
      if (quadCoef <= 0) {
        quadCoef = TAU
      }
      val delta = -y(i) * G(i) + y(j) * G(j)
      val oldAi = alpha(i)
      val oldAj = alpha(j)
      alpha(i) = alpha(i) + (-G(i) + y(i) * y(j) * G(j)) / quadCoef
      alpha(j) = alpha(j) - (-y(i) * y(j) * G(i) + G(j)) / quadCoef

      val sum = y(i) * oldAi + y(j) * oldAj

      if (alpha(i) > getC(i)) alpha(i) = getC(i)
      if (alpha(i) < 0) alpha(i) = 0
      alpha(j) = y(j) * (sum - y(i) * alpha(i))

      if (alpha(j) > getC(j)) alpha(j) = getC(j)
      if (alpha(j) < 0) alpha(j) = 0
      alpha(i) = y(i) * (sum - y(i) * alpha(j))
      val deltaAi = alpha(i) - oldAi
      val deltaAj = alpha(j) - oldAj

      for (t <- 1 until activeSize) {
        G(t) = G(t) + Q(t, i) * deltaAi + Q(t, j) * deltaAj
      }
    }
  }

  def isValidWorkingSet(i: Int, j: Int) = {
    j != -1
  }

  val G = Array(0.1)

  def selectWorkingSet = {
    var maxG = Double.MinValue
    var i = -1

    for (t <- 0 until len) {
      if (y(t) == +1) {
        if (!isUpperBound(t) && -G(t) >= maxG) {
          i = t
          maxG = -G(t)
        }
      } else {
        if (!isLowerBound(t) && G(t) >= maxG) {
          i = t
          maxG = G(t)
        }
      }
    }

    var j = -1
    var minObj = Double.MaxValue
    var minG = Double.MaxValue

    for (t <- 0 until len) {
      if ((y(i) == +1 && !isLowerBound(t)) ||
        (y(i) == -1 && !isUpperBound(t))) {
        val gradDiff = maxG + y(t) * G(t)
        if (-y(t) * G(t) <= minG) {
          minG = -y(t) * G(t)
        }
        if (gradDiff > 0) {
          val quadCoef = Q(i, i) + Q(t, t) - 2 * y(i) * y(t) * Q(i, t)
          val objDiff = if (quadCoef <= 0) {
            -(gradDiff * gradDiff) / TAU
          } else {
            -(gradDiff * gradDiff) / quadCoef
          }
          if (objDiff <= minObj) {
            j = t
            minObj = objDiff
          }
        }
      }
    }

    if (maxG - minG < epsilon) {
      (-1, -1)
    }
    (i, j)
  }

  private def getMaxIteration() = {
    val iter = if (len > Int.MaxValue / 100)
      Int.MaxValue // overflow
    else len * 100
    iter max 10000000 // We run at least 10000000 times
  }

  private def reconstructGradient {
    if (activeSize != len) {
      for (j <- activeSize until len) {
        G(j) = GBar(j) + p(j)
      }

      val nr_free = (0 until activeSize).count(isFree(_))
      if (2 * nr_free < activeSize) {
        // TODO
        println("WARNING: using -h 0 may be faster")
      }

      if (nr_free * len > 2 * activeSize * (len - activeSize)) {
        for {
          i <- activeSize until len
          j <- 0 until activeSize if isFree(j)
        } {
          G(i) += alpha(j) * Q(i, j)
        }
      } else {
        for {
          i <- 0 until activeSize if isFree(i)
          j <- activeSize until len
        } {
          G(j) += alpha(i) * Q(i, j)
        }
      }
    }
  }

  val GBar = Array(0.1)
}

object Solver {

  def solveOneClass(problem: SVMProblem, param: SVMParameter): Solution = {
    val n = (param.nu * problem.size).toInt

    val alpha = (0 until problem.size).map(_ match {
      case i if i < n => 1.0
      case i if i == n && i < problem.size => n
      case _ => 0.0
    }).toArray

    val zeros = Array.fill(problem.size)(0.0)
    val ones = Array.fill(problem.size)(1)

    val solver = new Solver(
      problem,
      param,
      new OneClassQMatrix(problem, param),
      zeros,
      ones,
      alpha,
      1.0,
      1.0);

    solver.solve
  }
}

class Solution(
  val obj: Double,
  val rho: Double,
  val upperBoundP: Double,
  val upperBoundN: Double,
  val r: Double) {
}

