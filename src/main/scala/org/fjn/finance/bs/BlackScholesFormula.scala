package org.fjn.finance.bs


object BlackScholesFormula{
  sealed trait BSPayoffType { def name: String }
  case object BS_CALL extends BSPayoffType { val name = "CALL" }
  case object BS_PUT extends BSPayoffType { val name = "PUT" }
}
trait BlackScholesFormula {

  import BlackScholesFormula._


  def npv(forward:Double, t:Double,T:Double,K:Double,s:Double,rf:Double,optionType:BlackScholesFormula.BSPayoffType):Double
  def delta(forward:Double, t:Double,T:Double,K:Double,s:Double,rf:Double,optionType:BlackScholesFormula.BSPayoffType):Double
  def gamma(forward:Double, t:Double,T:Double,K:Double,s:Double,rf:Double,optionType:BlackScholesFormula.BSPayoffType):Double
  def vega(forward:Double, t:Double,T:Double,K:Double,s:Double,rf:Double,optionType:BlackScholesFormula.BSPayoffType):Double
  def theta(forward:Double, t:Double,T:Double,K:Double,s:Double,rf:Double,optionType:BlackScholesFormula.BSPayoffType):Double
  def rho(forward:Double, t:Double,T:Double,K:Double,s:Double,rf:Double,optionType:BlackScholesFormula.BSPayoffType):Double

}
