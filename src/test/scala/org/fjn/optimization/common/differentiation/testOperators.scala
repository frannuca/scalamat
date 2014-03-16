package org.fjn.optimization.common.differentiation

import org.fjn.matrix.Matrix

object testOperators extends App{


  def f(x:Matrix[Double]):Double={
    val m = x.transpose*x
    m(0,0)
    2.0*x(0,0)*x(0,0)+3.0*x(1,0)*x(1,0)
  }

  val x0 = new Matrix[Double](2,1)
  x0 <= Seq(1.0,1.0)

  val ops = DifferentialOpsFactory(f _, x0.numberRows)

  val a = ops.grad(x0)
  println(a)
  require(math.abs(a(0,0)-4.0)<1e-2 && math.abs(a(1,0)-6.0)<1e-2)

  val b = ops.hessian(x0)
  println(b)
  require(math.abs(b(0,0)-4.0)<1e-2 && math.abs(b(1,1)-6.0)<1e-2 &&
    math.abs(b(1,0)) < 1e-2 && math.abs(b(0,1)) < 1e-2)


  //Taylor test:
  val xCentral = new Matrix[Double](2,1)
  xCentral <= Seq(1.0,1.0)
  def fTaylor = (x:Matrix[Double])=>{
    (new Matrix[Double](1,1) <= Seq(f(xCentral))) + (x-xCentral).transpose * ops.grad(xCentral) +  (x-xCentral).transpose * ops.hessian(xCentral) * (x-xCentral) * 0.5
  }

  val x1 = new Matrix[Double](2,1)<=Seq(0.75,-0.25)

  val taylor1 = fTaylor(x1)(0,0)
  val actual1 = f(x1)
  println("taylor = "+ taylor1)
  println ("actual = "+ actual1)
  require(math.abs((taylor1-actual1))<1e-3)


}
