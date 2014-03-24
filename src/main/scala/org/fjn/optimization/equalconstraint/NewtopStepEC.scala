package org.fjn.optimization.equalconstraint

import org.fjn.matrix.Matrix
import org.fjn.matrix.Scalar2MatrixConversions._
import org.fjn.optimization.common.differentiation.DifferentialOpsFactory

/**
 * Created by fran on 3/22/14.
 */
trait  NewtopStepEC {

  val A:Matrix[Double]
  val b:Matrix[Double]
  val dim:Int
  val f:(Matrix[Double])=>Double

  val diffOp = DifferentialOpsFactory(f,dim)
  lazy val zerosMatrix = new Matrix[Double](A.numberRows,A.numberRows).zeros

  def kktMatrix(x:Matrix[Double],nu:Option[Matrix[Double]]):(Matrix[Double],Matrix[Double],Double)={

    val gradf = diffOp.grad(x)
    val hessianf = diffOp.hessian(x)
    val At = A.transpose

    val kktDim = x.numberRows+A.numberRows

    val kkt = new Matrix[Double](kktDim , kktDim)

    kkt.setSubMatrix(hessianf,0,0)
    kkt.setSubMatrix(At,0,hessianf.numberCols)
    kkt.setSubMatrix(A,hessianf.numberRows,0)


    kkt.setSubMatrix(zerosMatrix,A.numberCols,A.numberCols)

    kkt.invert


    val left_side = new Matrix[Double](gradf.numberRows+A.numberRows,1)
    left_side.setSubMatrix(-1.0 * gradf,0,0)

    nu.map(_=>{
      left_side.setSubMatrix(-1.0 *(A* x - b),gradf.numberRows,0)
    })




    val r = kkt * left_side
    val dx = r.sub(0 until x.numberRows,Seq(0))
    val dnu = r.sub(x.numberRows until r.numberRows ,Seq(0))

    val lambda2 = dx.transpose * hessianf * dx

    (dx,dnu,lambda2(0,0))
  }

  def solve(x0:Matrix[Double],epsilon:Double):(Matrix[Double],Double)

}
