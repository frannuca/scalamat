package org.fjn.optimization.common.differentiation

import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq

trait DifferentialOperators{

  val pFunc:(org.fjn.matrix.Matrix[Double])=>Double
  val size:Int
  val dx = 1e-6

  val dOffsets: IndexedSeq[Matrix[Double]] = (0 until size).map(i =>{
    val dd = new Matrix[Double](size,1)
    dd.zeros
    dd.set(i,0,dx)
    dd
  })

  private def firstDerivative= (x:Matrix[Double],i:Int) =>{
    (pFunc(x + dOffsets(i))-pFunc( x - dOffsets(i)))/2.0/dx
  }

  private def secondDerivative= (x:Matrix[Double],i:Int,j:Int) =>{
    (firstDerivative(x+dOffsets(j),i)-firstDerivative(x-dOffsets(j),i))/2.0/dx
  }


  def grad(x:org.fjn.matrix.Matrix[Double]):Matrix[Double]={
    require(x.numberCols == 1 && x.numberRows == size)
    val g: IndexedSeq[Double] = (0 until size).map(i => firstDerivative(x,i))

    val mGrad = new Matrix[Double](size,1)
    mGrad <= g

    mGrad
  }

  def hessian(x:Matrix[Double]):Matrix[Double]={

    val mH = new Matrix[Double](size,size)

    for (i <- 0 until size;
         j <- 0 until size){
      mH.set(i,j,secondDerivative(x,i,j))
    }

    mH

  }
}
object DifferentialOpsFactory {


  def apply(ff:(Matrix[Double])=>Double,nDim:Int):DifferentialOperators={

    new {
      val size = nDim
      val pFunc = ff
    } with DifferentialOperators
  }

}
