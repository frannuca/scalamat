package org.fjn.risk.portfolio

import org.fjn.matrix.Matrix
import org.fjn.matrix._
import org.fjn.matrix.Scalar2MatrixConversions._
import org.fjn.matrix.MatrixExtensions._
import org.fjn.optimization.{MatrixType, IOptimizer}


/**
 * Created by fran on 3/9/14.
 */
case class QuadraticUtilityWithSum1(covariance:Matrix[Double],mean:Matrix[Double],phi:Double)
  extends UtilityFunction with IOptimizer{
  override def cost(x: Matrix[Double]): Double = {
    (x.transpose * mean - 0.5 * phi * (x.transpose * covariance * x))(0,0)
  }

  val c_inv = covariance.clone()
  c_inv.invert



  val ones = (0 until covariance.numberRows).map(_.asInstanceOf[Double]).toSeq.toMatrix
  val w = (ones.transpose * c_inv * ones)(0,0)

  override def solve(x0: MatrixType.DMatrix, functor: (MatrixType.DMatrix) => Double, tolerance: Double, maxIter: Int): (Matrix[Double], Double) ={


      val x = c_inv * ones / w + 1.0 / phi/w * ((w* c_inv * mean) - (ones.transpose *c_inv * mean)(0,0)*c_inv*ones)


      (x,cost(x))

  }


}
