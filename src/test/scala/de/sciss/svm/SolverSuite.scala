package de.sciss.svm
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.io.Source
import AssertUtil._
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class SolverSuite extends FunSuite {

  test("1 train case") {
    val param     = new SVMParameter(new LinearKernel)

    val source    = Source.fromString("-1\t1:1.0\t2:22.08\t3:11.46")
    val problem   = Problem.read(param, source)

    val solution  = Solver.solveOneClass(problem, param)
    svmAssertEquals( 77.482250, solution.obj)
    svmAssertEquals(309.929000, solution.rho)
    svmAssertEquals(Array(0.5), solution.alpha)
    svmAssertEquals(1, solution.upperBoundP)
    svmAssertEquals(1, solution.upperBoundN)
    svmAssertEquals(0, solution.r)
  }

  test("2 train case") {
    val param     = new SVMParameter(new LinearKernel)

    val source    = Source.fromString("-1\t1:1.0\t2:22.08\t3:11.46\n" +
                                      "+1\t1:2.0\t2:22.08\t3:11.46")
    val problem   = Problem.read(param, source)

    val solution  = Solver.solveOneClass(problem, param)
    svmAssertEquals(309.929000, solution.obj)
    svmAssertEquals(620.358000, solution.rho)
    svmAssertEquals(Array(1.0, 0.0), solution.alpha)
    svmAssertEquals(1, solution.upperBoundP)
    svmAssertEquals(1, solution.upperBoundN)
    svmAssertEquals(0, solution.r)
  }
}