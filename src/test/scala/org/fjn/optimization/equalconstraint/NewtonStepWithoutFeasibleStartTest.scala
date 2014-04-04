package org.fjn.optimization.equalconstraint


import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import scala.concurrent.ExecutionContext
import org.fjn.matrix.Matrix
import org.fjn.matrix.MatrixExtensions._

/**
 * Created by fran on 24.03.2014.
 */
class NewtonStepWithoutFeasibleStartTest extends AssertionsForJUnit {


  val dim = 5
  val A = new Matrix[Double](3,5) <= "3.0,4.0,1.0,1.1,4.4;" +
                                     "2.75,2.2,6.3,2.1,3.3;"+
                                     "1.75,0.2,3.3,2.1,7.3"

  val xs = Seq(2.2,2.2,2.2,2.2,2.2).toMatrix

  val b = A * xs

  val x0 = Seq(17.0,11.0,199.0,176.0,12.0).toMatrix
  val epsilon = 1e-6
  val u = Seq(1.0,2.0,3.0,4.0,5.0).toMatrix
  def f(x:Matrix[Double]):Double = {
    ((x-u).transpose * (x-u))(0,0)
  }
  @Test def testNotFeasibleStart{
    val obj = new NewtonStepWithoutFeasibleStart(A,b,5,f)

    val (rx,dv) = obj.solve(x0,epsilon)

    val r1 = A * rx - b
    val r2 = f(rx)

    var re2 =(r1.transpose*r1)(0,0)

    println(s"xs = $xs")
    println(s"rx = $rx")
    println(s"Ax = ${A * rx}")
    println(s" b = $b")
    println(s"Residue = $r1. The f(x)=${f(rx)}")
    println(s"Residue = $re2")


    assert(re2<1e-1)
  }
}
