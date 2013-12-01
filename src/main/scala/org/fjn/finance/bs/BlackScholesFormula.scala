package org.fjn.finance.bs


trait BlackScholesFormula {

  sealed trait BSPayoffType { def name: String }
  case object BS_CALL extends BSPayoffType { val name = "CALL" }
  case object BS_PUT extends BSPayoffType { val name = "PUT" }


  val optionType:BlackScholesFormula#BSPayoffType



  def npv(forward:Double, t:Double,T:Double,K:Double,s:Double,rf:Double):Double
  def delta(forward:Double, t:Double,T:Double,K:Double,s:Double,rf:Double):Double
  def gamma(forward:Double, t:Double,T:Double,K:Double,s:Double,rf:Double):Double
  def vega(forward:Double, t:Double,T:Double,K:Double,s:Double,rf:Double):Double
  def theta(forward:Double, t:Double,T:Double,K:Double,s:Double,rf:Double):Double
  def rho(forward:Double, t:Double,T:Double,K:Double,s:Double,rf:Double):Double

}
