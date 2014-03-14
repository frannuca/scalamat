package org.fjn.risk.portfolio

import org.fjn.matrix._

trait UtilityFunction {

  val covariance:Matrix[Double]
  val mean:Matrix[Double]
  val phi:Double


  def cost(x:org.fjn.matrix.Matrix[Double]):Double
}
