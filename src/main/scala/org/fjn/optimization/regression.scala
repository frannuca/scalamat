package org.fjn.optimization

import org.fjn.matrix.Matrix

case class Regression(order:Int) {
  def getRegressionCoefficients(points:Seq[(Double,Double)]):Seq[Double]={

    val Y  = (new Matrix[Double](points.size,1) <= points.map(_._2))

    val X = new Matrix[Double](points.size,order+1)
    for {
      i <- 0 until points.size;
      j <- 0 to order
    }{

      X.set(i,j,math.pow(points(i)._1,j))

    }

    val aux: Matrix[Double] = (X.transpose * X)
    aux.invert
    val aux2 = (aux * X.transpose * Y).getArray().toSeq
    aux2
  }
}
