package org.fjn.optimization.lineoptimizer

import org.apache.commons.math3.optimization.univariate.BrentOptimizer
import org.apache.commons.math3.optimization.GoalType
import org.apache.commons.math3.analysis.UnivariateFunction

/**
 * Created by fran on 3/23/14.
 */
object LineBrent {


  def apply(f:(Double)=>Double,linearSearchIteration:Int,minVal:Double,maxVal:Double,startVal:Double): Double = {
    val uf = new UnivariateFunction{
      override def value(x: Double): Double = f(x)
    }
    val brent = new BrentOptimizer(0.01,1e-3)
    val lr = brent.optimize(linearSearchIteration,uf, GoalType.MINIMIZE,minVal,maxVal,startVal)

    lr.getPoint
  }



}
