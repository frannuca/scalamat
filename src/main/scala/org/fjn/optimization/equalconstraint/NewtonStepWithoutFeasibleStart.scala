package org.fjn.optimization.equalconstraint

import org.fjn.matrix.Matrix
import org.fjn.matrix.Scalar2MatrixConversions._
import org.fjn.matrix.MatrixExtensions._

import org.fjn.optimization.lineoptimizer.LineBrent

/**
 * Created by fran on 3/23/14.
 */
case class NewtonStepWithoutFeasibleStart(A:Matrix[Double],b:Matrix[Double],dim:Int,f:(Matrix[Double])=>Double )
  extends NewtopStepEC{

  val At =  A.transpose

  def flineal(t:Double)(x:Matrix[Double],dx:Matrix[Double])(v:Matrix[Double],dv:Matrix[Double]):Double={
    val rdual = diffOp.grad(x + t * dx ) + At * (v+t*dv)
    val rpri = A*(x + t * dx) - b

    (rdual.transpose * rdual)(0,0)+ (rpri.transpose * rpri)(0,0)
  }
  override def solve(x0: Matrix[Double], epsilon: Double): (Matrix[Double], Double) = {

    var tol = 1e9
    var x = x0.clone()
    var v = new Matrix[Double](A.numberRows,1).random

    for(i <- 0 until 100 if tol > epsilon){


      val (dx,dv,l2) = kktMatrix(x0,Some(v))

      val t = LineBrent(flineal(_)(x,dx)(v,dv),20,0.01,1.0,0.2)


      x = x + t * dx
      v = v + t * dv

      val res = (A*x - b)
      tol = (res.transpose * res)(0,0)
    }

    (x,f(x))
  }
}