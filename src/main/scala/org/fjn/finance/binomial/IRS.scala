package org.fjn.finance.binomial


case class ForwardRate(r1:Double,r2:Double,t1:Double,t2:Double){
  val d1 =  math.exp(r1*t1)
  val d2 = math.exp(r2*t2)
  val fwdRate =  math.log(d2/d1)/(t2-t1)
}

case class IRS(Ts:Double,Te:Double) {

}
