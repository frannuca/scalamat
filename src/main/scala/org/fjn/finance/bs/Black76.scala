package org.fjn.finance.bs

object Black76 extends BlackScholesFormula {


  import BlackScholesFormula._

  private val stdDist = new org.apache.commons.math3.distribution.NormalDistribution()
  private val PHI = (x:Double) => stdDist.cumulativeProbability(x)

  private def computeAuxiliary(ft:Double, t:Double,T:Double,K:Double,s:Double)={

    val dt = T-t
    val vol = s*math.sqrt(dt)



    val d1 = (math.log(ft/K)+0.5*s*s*dt)/vol
    val d2 = d1-vol


    (d1,d2,dt)
  }

  def npv(forward: Double, t: Double, T: Double, K: Double, s: Double, rf: Double,optionType:BlackScholesFormula.BSPayoffType): Double = {

    val (d1,d2,dt) = computeAuxiliary(forward,t,T,K,s)
    optionType match{
      case  BS_CALL => math.exp(-rf*dt)*(forward*PHI(d1)-K*PHI(d2))
      case _ =>        math.exp(-rf*dt)*(K*PHI(-d2)-forward*PHI(-d1))
    }
  }

  def delta(forward: Double, t: Double, T: Double, K: Double, s: Double, rf: Double,optionType:BlackScholesFormula.BSPayoffType): Double ={
    val (d1,d2,dt) = computeAuxiliary(forward,t,T,K,s)
    optionType match{
      case  BS_CALL => math.exp(-rf*dt)*PHI(d1)
      case _ =>        math.exp(-rf*dt)*(PHI(d1)-1.0)
    }
  }

  def gamma(forward: Double, t: Double, T: Double, K: Double, s: Double, rf: Double,optionType:BlackScholesFormula.BSPayoffType): Double = {
    val (d1,d2,dt) = computeAuxiliary(forward,t,T,K,s)

    math.exp(-rf*dt)*PHI(d1)/(forward*s*math.sqrt(dt))


  }

  def vega(forward: Double, t: Double, T: Double, K: Double, s: Double, rf: Double,optionType:BlackScholesFormula.BSPayoffType): Double = {

    val (d1,d2,dt) = computeAuxiliary(forward,t,T,K,s)
    forward* math.exp(-rf*dt)  *PHI(d1)*math.sqrt(dt)
  }

  def theta(forward: Double, t: Double, T: Double, K: Double, s: Double, rf: Double,optionType:BlackScholesFormula.BSPayoffType): Double = {
    val (d1,d2,dt) = computeAuxiliary(forward,t,T,K,s)

    val a = math.exp(-rf*dt)

    optionType match{
      case BS_CALL =>
        -forward * a *PHI(d1)*s*0.5/math.sqrt(dt)
        +rf*forward*a*PHI(-d1)
        -rf*K*a*PHI(-d2)

      case _=>
        -forward * a *PHI(d1)*s*0.5/math.sqrt(dt)
        -rf*forward*a*PHI(-d1)
        +rf*K*a*PHI(-d2)
    }

  }

  def rho(forward: Double, t: Double, T: Double, K: Double, s: Double, rf: Double,optionType:BlackScholesFormula.BSPayoffType): Double = {
    val (d1,d2,dt) = computeAuxiliary(forward,t,T,K,s)

    optionType match{
      case BS_CALL=>
        dt*K*math.exp(-rf*dt)*PHI(d2)
      case _=>
        -dt*K*math.exp(-rf*dt)*PHI(-d2)
    }
  }
}

