package org.fjn.optimization.common

import org.fjn.matrix.Matrix


object MatrixType {type DMatrix = org.fjn.matrix.Matrix[Double]}

trait IOptimizerSolver {


  val data:IOptimizerData
  protected def solve(x0:org.fjn.matrix.Matrix[Double],d:IOptimizerData):(Matrix[Double],Double)
  def run(x0:Matrix[Double]):(Matrix[Double],Double)={
    solve(x0,data)
  }

}


case class IOptimizerSolverBuilder(){


  private val oData = IOptimizerDataBuilder()
  private var fsolver:(Matrix[Double],IOptimizerData) => (Matrix[Double], Double) = _

  def withTolerance(tol:Double)={
    oData.withTolerance(tol)
    this
  }

  def withMaxIterations(niter:Int)={
    oData.withMaximumNumberOfIteration(niter)
    this
  }

  def withCostFunction(f:(Matrix[Double]=>Double))={
    oData.withCostFunction(f)
    this
  }

  def withConstraint(c:Constraint)={
    oData.withConstraint(c)
    this
  }

  def withSolver(solve:(Matrix[Double],IOptimizerData) =>  (Matrix[Double],Double))={
    fsolver =   solve
    this
  }



  def build={
    new IOptimizerSolver {
      override val data: IOptimizerData = oData.build
      override def solve(x0:org.fjn.matrix.Matrix[Double],d:IOptimizerData):(Matrix[Double],Double) = fsolver(x0,data)
    }
  }

}