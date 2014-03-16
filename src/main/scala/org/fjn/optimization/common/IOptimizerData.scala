package org.fjn.optimization.common

import org.fjn.matrix.Matrix
import scala.collection.mutable.ListBuffer

trait IOptimizerData {
  val constraints:Seq[Constraint]
  val cost:(Matrix[Double])=>Double
  val tolerance:Double
  val maxIterations:Int
  }


case class  IOptimizerDataBuilder(){
  private var fconstraints = new ListBuffer[Constraint]()
  private var fcost:(Matrix[Double]=>Double) = x => 0.0
  private var tol:Double=1e-4
  private var maxIter = 1

  def withConstraint(c:Constraint)={
    fconstraints += c
    this
  }

  def withCostFunction(f:(Matrix[Double]=>Double))={
    fcost=f
    this
  }

  def withTolerance(t:Double)={
    tol = t
    this
  }

  def withMaximumNumberOfIteration(n:Int)={
    maxIter = n
    this
  }
  
  def build: IOptimizerData={
    new IOptimizerData {
      override val constraints: Seq[Constraint] = fconstraints
      override val cost = fcost
      override val tolerance = tol
      override val maxIterations = maxIter

    }
  }
}
