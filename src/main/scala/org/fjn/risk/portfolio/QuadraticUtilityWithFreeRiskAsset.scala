//package org.fjn.risk.portfolio
//
//import org.fjn.matrix.Matrix
//
//import org.fjn.matrix.MatrixExtensions._
//import org.fjn.matrix.Scalar2MatrixConversions._
//import org.fjn.optimization.common.{MatrixType, IOptimizerSolver}
//
//case class QuadraticUtilityWithFreeRiskAsset (covariance:Matrix[Double],mean:Matrix[Double],phi:Double)
//  extends UtilityFunction with IOptimizerSolver{
//  override def cost(x: Matrix[Double]): Double = {
//    (x.transpose * mean - 0.5 * phi * (x.transpose * covariance * x))(0,0)
//  }
//
//
//  val r = mean(mean.numberRows-1,0)
//
//  val c_1 = covariance.sub((0 until covariance.numberRows-1).toSeq,(0 until covariance.numberCols-1).toSeq)
//  val c_inv = c_1.clone()
//  val mean_1 = mean.sub((0 until mean.numberRows-1).toSeq,Seq(0))
//  c_inv.invert
//
//
//
//  val ones = (0 until c_1.numberRows).map(_.asInstanceOf[Double]).toSeq.toMatrix
//  val w = (ones.transpose * c_inv * ones)(0,0)
//
//  override def solve(x0: MatrixType.DMatrix, functor: (MatrixType.DMatrix) => Double, tolerance: Double, maxIter: Int): (Matrix[Double], Double) ={
//
//
//    var phi_ = phi
//
//    val m_r = mean_1 - r * ones
//    val sc = ones.transpose * c_inv * m_r
//    val alpha = 1.0/phi_ * ones.transpose * c_inv * m_r
//    val x0 = c_inv * m_r/sc(0,0)
//
//    val x =  (x0.getArray().map(v => v * alpha(0,0)).toSeq ++ Seq(1-alpha(0,0))).toMatrix
//
//    println(x.toString)
//    (x,cost(x))
//
//  }
//
//}