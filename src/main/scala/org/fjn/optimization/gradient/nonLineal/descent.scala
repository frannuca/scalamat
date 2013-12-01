package org.fjn.optimization.gradient.nonLineal

import org.fjn.matrix.Matrix
import org.fjn.optimization.gradient.differentiation.DifferentialOpsFactory
import org.apache.commons.math3.optimization.univariate.BrentOptimizer
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.optimization.GoalType


class GradientDescent(pFunc:(Matrix[Double])=>Double,x0:Matrix[Double],tol:Double=1e-3,gradTol:Double=1e-3) {

  implicit def toUniFunc(f:(Double)=>Double)={
    new UnivariateFunction {
      def value(x: Double): Double = f(x)
    }
  }
  import org.fjn.matrix.MatrixExtensions._
  import org.fjn.matrix.Scalar2MatrixConversions._

  val linearSearchIteration:Int=50
  val dx =  x0.getArray().map(_ => 1e-6).toSeq.toMatrix

  val diffOp = DifferentialOpsFactory(pFunc,x0.numberRows)


  def checkConvergence(x:Matrix[Double],xOld:Matrix[Double],grad:Matrix[Double])={
    val dx =((x-xOld)*(x-xOld).transpose)(0,0)
    val nGrad = (grad*grad.transpose)(0,0)

    dx<tol || nGrad<gradTol
  }
  def ++(numberOfIterations:Int):Matrix[Double]={

    var x = x0
    var xOld = x

      for(i<-0 until numberOfIterations){

        val g = diffOp.grad(x)


        val fLinear = (v:Double)=>{pFunc(x-g*v)}
        val brent = new BrentOptimizer(0.01,1e-3)
        val lr = brent.optimize(linearSearchIteration,fLinear,GoalType.MINIMIZE,1e-5,1,1e-5)
        xOld = x
        x += -g * lr.getPoint

        if(checkConvergence(x,xOld,g))
          return x

      }
    x
  }
}
