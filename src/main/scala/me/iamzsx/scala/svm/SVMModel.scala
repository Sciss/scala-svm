package me.iamzsx.scala.svm

import java.io._
import scala.math._
import scala.io._

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object SVMType extends Enumeration {
  type SVMType = Value
  val C_SVC = Value("c_svc")
  val NU_SVC = Value("nu_svc")
  val ONE_CLASS = Value("one_class");
  val EPSILON_SVR = Value("epsilon_svr");
  val NU_SVR = Value("nu_svr");
}

import SVMType._

class SVMNode(
  val index: Int,
  val value: Double) {

  override def toString = (index, value).toString
}

object SVMNode {
  def apply(index: Int, value: Double) = new SVMNode(index, value)
}

class SVMParameter(
  val svmType: SVMType,
  val kernel: Kernel,
  val nu: Double,
  val eps: Double,
  val shrinking: Boolean) {

  var gamma = 0.0

  override def toString = Array(
    "svm_type " + svmType, kernel.toString).mkString("\n")
}

class EpsilonSVRSVMParamter(
  svmType: SVMType,
  kernel: Kernel,
  nu: Double,
  eps: Double,
  shrinking: Boolean,
  val C: Double,
  val p: Double) extends SVMParameter(svmType, kernel, nu, eps, shrinking) {

}

abstract class SVMModel(
  val param: SVMParameter,
  val SV: Array[List[SVMNode]],
  val rho: Array[Double]) {

  def predict(x: List[SVMNode]) = {
    predict_values(x)
  }

  def predict_values(x: List[SVMNode]): (Double)

  override def toString = Array(
    param.toString,
    "total_sv " + SV.size,
    "rho " + rho.mkString(" ")).mkString("\n")
}

class BaseModel(
  param: SVMParameter,
  SV: Array[List[SVMNode]],
  rho: Array[Double]) extends SVMModel(param, SV, rho) {

  def predict_values(x: List[SVMNode]): Double = {
    SV.map(param.kernel(x, _)).reduce(_ + _) - rho(0);
  }
}

class OneClassModel(
  param: SVMParameter,
  SV: Array[List[SVMNode]],
  rho: Array[Double]) extends BaseModel(param, SV, rho) {

  override def predict_values(x: List[SVMNode]): Double = {
    if (super.predict_values(x) > 0) 1 else -1
  }

}

class EpsilonSVRModel(
  param: SVMParameter,
  SV: Array[List[SVMNode]],
  rho: Array[Double]) extends BaseModel(param, SV, rho) {
}

class NUSVRModel(
  param: SVMParameter,
  SV: Array[List[SVMNode]],
  rho: Array[Double]) extends BaseModel(param, SV, rho) {
}

class CSVCModel {

}

class NUSVCModel {

}

object SVMModel {

  def load(file: String): SVMModel = {
    for (line <- Source.fromFile(file).getLines) {
      val splits = line.split(" ")
      splits(0) match {
        case "svm_type" => ""
        case "kernel_type" => ""
        case "degree" => ""
        case "gamma" => ""
        case "coef0" => ""
        case "nr_class" => ""
        case "total_sv" => ""
        case "rho" => ""
        case "label" => ""
        case "probA" => ""
        case "probB" => ""
        case "nr_sv" => ""
        case "SV" => ""
      }
    }
    null
  }

  def save(file: String) {
    val output = new PrintWriter(new FileWriter(file))

    output.close()
    //val output = Resource.fromFile(file)(Codec.UTF8)
  }
}

class SolutionInfo {
  val upper_bound_p: Double = 0;
  val upper_bound_n: Double = 0;
  val rho: Double = 0;
}

class DecisionFunction(val alpha: Array[Double], val rho: Double)

class SVMProblem(val param: SVMParameter, val y: Array[Double], val x: Array[Array[SVMNode]]) {
  require(y.size == x.size)

  val size = y.size

  def train_one(Cp: Double, Cn: Double) = {
    val alpha = Array(0.0)

    param.svmType match {
      case C_SVC =>
        solve_c_svc
      case NU_SVC =>
        solve_nu_svc
      case ONE_CLASS =>
        solve_one_class
      case EPSILON_SVR =>
        solve_epsilon_svr
      case NU_SVR =>
        solve_nu_svr
    }

    val si = new SolutionInfo
    val nSV = alpha.filter(abs(_) > 0).size
    val nBSV =
      0 until alpha.size filter (i => abs(alpha(i)) > 0 &&
        (y(i) > 0 && abs(alpha(i)) >= si.upper_bound_p) ||
        (y(i) <= 0 && abs(alpha(i)) >= si.upper_bound_n)) size

    new DecisionFunction(alpha, si.rho)
  }

  def train {

  }

  def solve_c_svc {

  }

  def solve_nu_svc {

  }

  def solve_one_class {

  }

  def solve_epsilon_svr {

  }

  def solve_nu_svr {

  }

}

object SVMProblem {

  def get(param: SVMParameter, source: Source) = {
    val y = ArrayBuffer[Double]()
    val x = ArrayBuffer[Array[SVMNode]]()
    var maxIndex = 0
    for (line <- source.getLines.map(_.trim)) {
      val splits = line.split('\t')
      if (splits.size <= 1) {
        // Do we need to support no feature?
        throw new IOException("Invalid input: " + line)
      }
      val (Array(label), features) = splits.splitAt(1)
      y += label.toDouble
      var featureMaxIndex = 0
      x += features.map((feature: String) => {
        val splits = feature.split(':')
        if (splits.size != 2) {
          throw new IOException("Invalid input: " + line)
        }
        val index = splits(0).toInt
        if (index <= featureMaxIndex) {
          throw new IOException("Index must be in order and unique: " + line)
        } else {
          featureMaxIndex = index
        }
        val value = splits(1).toDouble
        SVMNode(index, value)
      })
      maxIndex = maxIndex max featureMaxIndex
    }
    if (param.gamma == 0 && maxIndex > 0) {
      param.gamma = 1.0 / maxIndex
    }
    new SVMProblem(param, y.toArray, x.toArray)
  }

}

