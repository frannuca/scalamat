package org.fjn.statistics

import org.fjn.matrix.Matrix

import org.fjn.matrix.MatrixExtensions._

object Estimators {

  /**
   * x is a matrix N x K, containing K column vector (independent variables) with N samples
   * @param x  matrix of K independent variable with N sampleseach
   * @return mean vector Kx1 containing the mean of each of the K independent variables
   */
  def mean(x:Matrix[Double]):Matrix[Double]={

    (for(j <- 0 until x.numberCols)yield{
       x.sub(0 until x.numberRows,Seq(j)).getArray().foldLeft(0d)((acc,r)=> acc + r) * 1.0/x.numberRows
     }).toSeq.toMatrix

  }

  /**
   * computes the covariance matrix given two matrix of N samples x K independent variables
   * @param x matrix of N samples x K indpendent variables
   * @return Covariance Matrix K x K
   */
  def covariance(x:Matrix[Double]):Matrix[Double]={


    val muX = mean(x) // K x 1

    val N= x.numberRows
    val K = x.numberCols

   val cov= new Matrix[Double](x.numberCols,x.numberCols)

    for{i <- 0 until K
        j <- 0 until K
    }{
      cov(i,j)= ((x.sub(0 until N,Seq(i)) - muX(i,0)) * (x.sub(0 until N,Seq(j)) - muX(j,0)).transpose)(0,0)
    }

    cov * 1.0/(x.numberRows - 1.0)
  }

  def correlation(x:Matrix[Double]):Matrix[Double]={
    val cov = covariance(x)
    val sigmas = cov.diag.map(x => math.sqrt(x))

    for {i<- 0 until cov.numberRows
         j <- 0 until cov.numberCols}{
      cov(i,j)= cov(i,j)/(sigmas(i)*sigmas(j))
    }

    cov
  }


}
