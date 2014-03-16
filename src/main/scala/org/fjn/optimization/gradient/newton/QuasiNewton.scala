package org.fjn.optimization.gradient.newton

import org.fjn.optimization.common.differentiation.{DifferentialOpsFactory, DifferentialOperators}
import org.fjn.matrix.Matrix
import org.fjn.optimization.common.{IOptimizerData, IOptimizerSolverBuilder, MatrixType}
import MatrixType.DMatrix



sealed trait QNUpdateType
object QNUPDATE_TYPES{
  case object  DFPQNUPDATE extends QNUpdateType
  case object  BFGSQNUPDATE extends QNUpdateType
}

trait QNUpdate{
  def next(Hk:Matrix[Double],dx:Matrix[Double],dy:Matrix[Double]):Matrix[Double]
}
trait DFPQNUpdate extends QNUpdate{

  def next(Hk:Matrix[Double],dx:Matrix[Double],dy:Matrix[Double]):Matrix[Double]={


    val dxT = dx.transpose
    val HkT = Hk.transpose
    val dyT = dy.transpose

    val aux1: Double = ( dyT * dx )(0,0)
    val aux2 = (dyT * Hk * dy)(0,0)

     Hk +  dx * dxT / aux1 - Hk * dy * dyT * HkT / aux2
  }
}

trait BFGSQNUpdate extends QNUpdate{

  def next(Hk:Matrix[Double],dx:Matrix[Double],dy:Matrix[Double]):Matrix[Double]={


    val dxT = dx.transpose
    val HkT = Hk.transpose
    val dyT = dy.transpose
    val I = new Matrix[Double](dx.numberRows,dx.numberRows)
    I.eye

    val aux1 = (dyT*dx)(0,0)
    val term1 = (I - dy * dxT /aux1)

    term1.transpose  * Hk * term1 + dx * dxT/aux1

  }
}



trait QuasiNewton  {

  self:QNUpdate =>

  val alpha0:Double
  def solve(x0:Matrix[Double],data:IOptimizerData):(Matrix[Double],Double)={

    var alpha = alpha0


    val tolerance = x0.getArray().indices.toSeq.map( _ =>data.tolerance)


    val ops= DifferentialOpsFactory( data.cost ,x0.numberRows)

    var Hk = new Matrix[Double](x0.numberRows,x0.numberRows)
    Hk.eye
    var x = x0.clone()

    var minX = x0.clone
    //first iteration:
    var grad0 = ops.grad(x0)
    var dx  =  Hk * grad0 * (-alpha)
    x = x0 + dx
    var grad1 = ops.grad(x)

    var counter = 0
    (0 until data.maxIterations).foreach(i =>{

      val xOld = x
      val HkOld = Hk
      val dxOld = dx

      Hk = self.next(Hk,dx,grad1-grad0)

      dx = Hk*grad1*(-alpha)


      val oldFunc = data.cost(x)

      x = x + dx
      val eval = data.cost(x)
      if (eval>oldFunc || eval.isNaN){
        x = xOld
        Hk = HkOld
        dx = dxOld
        alpha = alpha * 0.9
        counter = counter + 1

      }else{
        minX = x.clone()
        counter = 0
        grad0 = grad1
        grad1 = ops.grad(x)
      }



    })

    (x,data.cost(x))
}
}


object QuasiNewtonSolver{
  def apply(x0:Matrix[Double],f:(Matrix[Double]=>Double),tolerance:Double,maxIter:Int,t:QNUpdateType,alpha:Double)={

    import QNUPDATE_TYPES._

    val qnewton1 = t match{
      case DFPQNUPDATE =>  new  {override val alpha0 = alpha} with QuasiNewton with DFPQNUpdate
      case BFGSQNUPDATE => new  {override val alpha0 = alpha} with QuasiNewton with BFGSQNUpdate
      case _ => throw new Throwable("Invalid Hessian update type")
    }
    IOptimizerSolverBuilder().withCostFunction(f).withMaxIterations(maxIter).withSolver(qnewton1.solve).build
  }
}
