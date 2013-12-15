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
       x.sub(0 until x.numberRows,Seq(j))
     }).foldLeft(new Matrix[Double](x.numberRows,1))((acc,r)=> acc + r) * 1.0/x.numberCols

  }


  /**
   * computes the covariance matrix given two matrix of N samples x K independent variables
   * @param x matrix of N samples x K indpendent variables
   * @return Covariance Matrix K x K
   */
  def covariance(x:Matrix[Double]):Matrix[Double]={


    val x0 = x.clone()
    val muX = mean(x) // K x 1

    val N= x.numberRows
    val M = x.numberCols

    for{ j <- 0 until M
         i <- 0 until N}{
        x0(i,j)=x(i,j)-muX(i,0)
    }

   (x0 * x0.transpose)/(M-1)

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
