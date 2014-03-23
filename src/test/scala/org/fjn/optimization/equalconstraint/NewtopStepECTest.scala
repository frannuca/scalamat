package org.fjn.optimization.equalconstraint

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import scala.concurrent.ExecutionContext
import org.fjn.matrix.Matrix

/**
 * Created by fran on 3/22/14.
 */
class NewtopStepECTest extends AssertionsForJUnit{


  val dim = 5
  val A = new Matrix[Double](3,5)
  val b = new Matrix[Double](3,1)
  val x0 = new Matrix[Double](5,5)
  val epsilon = 1e-3


  def cost(x:Matrix[Double]):Double={
    (x.transpose * x)(0,0)
  }
  @Test def testKKTEqualConstraintWithFeasibleStartPoint{
    val opt = new NewtonStepWithFeasibleStart(A,b,dim,cost)

    val (x,fx) = opt.solve(x0,epsilon)

  }
}
