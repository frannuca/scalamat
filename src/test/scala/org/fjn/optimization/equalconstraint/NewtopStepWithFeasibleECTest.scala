package org.fjn.optimization.equalconstraint

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import scala.concurrent.ExecutionContext
import org.fjn.matrix.Matrix
import org.fjn.matrix.Matrix
import org.fjn.matrix.MatrixExtensions._

/**
 * Created by fran on 3/22/14.
 */
class NewtopStepWithFeasibleECTest extends AssertionsForJUnit{


  val dim = 5
  val A = new Matrix[Double](3,5) <= "3.0,4.0,1.0,1.1,4.4;" +
    "2.75,2.2,6.3,2.1,3.3;"+
    "1.75,0.2,3.3,2.1,7.3"

  val xs = Seq(2.2,2.2,2.2,2.2,2.2).toMatrix

  val b = A * xs

  val x0 = xs
  val epsilon = 1e-6

  val u = Seq(1.0,2.0,3.0,4.0,5.0).toMatrix

  def cost(x:Matrix[Double]):Double={
    ((x-u).transpose * (x-u))(0,0)
  }
  @Test def testKKTEqualConstraintWithFeasibleStartPoint{
    val opt = new NewtonStepWithFeasibleStart(A,b,dim,cost)

    val (x,fx) = opt.solve(x0,epsilon)

    println(s"x=$x,\nf(x)=$fx")
    println(s"Ax=${A*x}")
    println(s"b=${b}")


  }
}
