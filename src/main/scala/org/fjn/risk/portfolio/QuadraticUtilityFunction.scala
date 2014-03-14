package org.fjn.risk.portfolio

import org.fjn.matrix.Matrix
import org.fjn.matrix.Scalar2MatrixConversions._
import org.fjn.matrix.MatrixExtensions._
import org.fjn.optimization.{MatrixType, IOptimizer}
import org.fjn.optimization.gradient.nonLineal.GradientDescent


/**
 * Created by fran on 3/9/14.
 */
case class QuadraticUtilityFunction(covariance:Matrix[Double],mean:Matrix[Double],phi:Double)
  extends UtilityFunction with GradientDescent{

  override def cost(x: Matrix[Double]): Double = {
    (x.transpose * mean - 0.5 * phi * x.transpose * covariance * x)(0,0)
  }

  def getWeights(x0:Matrix[Double]): (Matrix[Double], Double) ={
    solve(x0,cost,1e-5,100)
  }

}
