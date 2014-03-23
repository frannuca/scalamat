package org.fjn.optimization.equalconstraint

import org.fjn.matrix._
import org.fjn.matrix.Scalar2MatrixConversions._
import org.fjn.matrix.MatrixExtensions._
import org.fjn.optimization.lineoptimizer.LineBrent

/**
 * Created by fran on 3/23/14.
 */
case class NewtonStepWithFeasibleStart(A:Matrix[Double],b:Matrix[Double],dim:Int,f:(Matrix[Double])=>Double )
  extends NewtopStepEC{


  def flineal(t:Double)(x:Matrix[Double],dx:Matrix[Double]):Double={
    f(x + t * dx)
  }
  override def solve(x0: Matrix[Double], epsilon: Double): (Matrix[Double], Double) = {

    var tol = 1e9
    var x = x0.clone()
    for(i <- 0 until 100 if tol > epsilon){


      val (dx,_,l2) = kktMatrix(x0,None)

      val t = LineBrent(flineal(_)(x,dx),20,0.01,1.0,0.2)


      tol = 0.5 * l2

      x = x + t * dx
    }

    (x,f(x))
  }
}
