package org.fjn.finance.volatility

import org.fjn.finance.bs.{BlackScholesFormula, Black76}
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.optimization.univariate.{UnivariatePointValuePair, BrentOptimizer}
import org.apache.commons.math3.optimization.GoalType
import org.fjn.matrix.Matrix
import org.fjn.optimization.gradient.nonLineal.GradientDescent


case class Point(s:Double,t:Double)

class ImpliedVolatilitySurface(callPricess:Seq[Point],putPrices:Seq[Point]) {

  def zeroFunctor_call(sigma:Double)(npv:Double,spot:Double,strike:Double,t:Double,T:Double,r:Double)={
    val  z = math.log(Black76.npv(spot,t,T,strike,sigma,r,BlackScholesFormula.BS_CALL))- math.log(npv)
    z*z
  }


  def zeroFunctor_put(sigma:Double)(npv:Double,spot:Double,strike:Double,t:Double,T:Double,r:Double)={
    val  z = math.log(Black76.npv(spot,t,T,strike,sigma,r,BlackScholesFormula.BS_PUT))- math.log(npv)
    z*z
  }


  implicit def toUniFunc(f:(Double)=>Double)={
    new UnivariateFunction {
      def value(x: Double): Double = f(x)
    }
  }


  import org.fjn.matrix.MatrixExtensions._
  val brent = new BrentOptimizer(0.01,1e-5)
  private def solve(npv:Double,
                    spot:Double,
                    strike:Double,
                    t:Double,
                    T:Double,
                    r:Double,
                    optionType:BlackScholesFormula.BSPayoffType)
                   (nIteration:Int): Double ={


    val bs = optionType match{
      case BlackScholesFormula.BS_CALL=> (s:Matrix[Double]) => zeroFunctor_call(s(0,0))(npv,spot,strike,t,T,r)
      case _ => (s:Matrix[Double]) => zeroFunctor_put(s(0,0))(npv,spot,strike,t,T,r)
    }
    import org.fjn.matrix.MatrixExtensions._
    val gd = new GradientDescent{

    }
    val rVol = gd.solve(Seq(0.01).toMatrix,bs,1e-3,100)

    rVol._2
  }

}
