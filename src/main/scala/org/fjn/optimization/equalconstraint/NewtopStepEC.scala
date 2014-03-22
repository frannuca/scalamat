package org.fjn.optimization.equalconstraint

import org.fjn.matrix.Matrix
import org.fjn.matrix.Scalar2MatrixConversions._
import org.fjn.optimization.common.differentiation.DifferentialOpsFactory

/**
 * Created by fran on 3/22/14.
 */
case class NewtopStepEC(A:Matrix[Double],b:Matrix[Double],dim:Int,f:(Matrix[Double])=>Double) {

  val diffOp = DifferentialOpsFactory(f,dim)

  def kktMatrix(x:Matrix[Double]):(Matrix[Double],Double)={
    val gradf = diffOp.grad(x)
    val hessianf = diffOp.hessian(x)
    val At = A.transpose

    val kktDim = x.numberRows+A.numberRows
    val kkt = new Matrix[Double](kktDim , kktDim)
    kkt.setSubMatrix(hessianf,0,0)
    kkt.setSubMatrix(At,0,hessianf.numberCols)
    kkt.setSubMatrix(A,hessianf.numberRows,0)
    kkt.setSubMatrix(new Matrix[Double](A.numberRows,A.numberRows).zeros,hessianf.numberRows,hessianf.numberCols)

    val b = new Matrix[Double](gradf.numberRows,1)
    b.setSubMatrix(-1.0 * gradf,0,0)

    kkt.invert

    val r = kkt * b

    val dx = r.sub((0 until x.numberRows),Seq(0))


    val lambda2 = (dx.transpose * hessianf * dx)(0,0)


    (x+dx,lambda2)

  }

  def solve(x0:Matrix[Double],epsilon:Double):(Matrix[Double],Double)={


    var tol = 1e9
    var x = x0
    for(i <- 0 until 100 if tol < epsilon){

      var (xnew,l2) = kktMatrix(x)
      tol = 0.5*l2

      x = xnew
    }

    (x,f(x))
  }

}
