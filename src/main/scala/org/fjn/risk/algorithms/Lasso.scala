package org.fjn.risk.algorithms

import org.fjn.matrix._

import org.fjn.matrix.Scalar2MatrixConversions._
import org.apache.commons.math3.optimization.GoalType
import org.apache.commons.math3.optimization.univariate.BrentOptimizer
import org.apache.commons.math3.analysis.UnivariateFunction


case class Lasso(X:Matrix[Double],y:Matrix[Double],lambda:Double) {


  private def H(beta:Matrix[Double]): Double ={


    (0.5 * (y - X* beta).transpose * (y - X* beta) + lambda * (beta.transpose * beta))(0,0)
  }

  private def H1D(X_i:Matrix[Double],xi:Matrix[Double],B_i:Matrix[Double],bi:Double): Double ={
    val yi = y - X_i * B_i
    0.5 * ((yi - xi*bi).transpose *  (yi - xi*bi))(0,0) + lambda * math.abs(bi)  + lambda * B_i.getArray().map(math.abs(_)).sum
  }




  def solve(beta0:Matrix[Double]):Matrix[Double]={

    var beta = beta0.clone()

    for(k <- 0 until y.numberRows){

      val fk =  H(beta)

      for( i<- 0 until X.numberCols){

        val beta_i = beta.sub( (0 until beta.numberRows).filter(j => j != i), Seq(0) )
        val X_i = X.sub( (0 until X.numberRows),(0 until X.numberRows).filter(j => j != i) )

        val fLinear = new UnivariateFunction{
          override def value(v: Double): Double = {
            H1D(X_i, X.sub( (0 until X.numberRows),Seq(i)),beta_i,beta(0,i))
          }
        }
        val brent = new BrentOptimizer(0.01,1e-3)
        val b_i_opt = brent.optimize(100,fLinear,GoalType.MINIMIZE,0,1000)

        beta(0,i) = b_i_opt.getPoint
      }
    }

    beta
  }
}
