package org.fjn.optimization.gradient.newton

import org.fjn.matrix.Matrix
import org.fjn.matrix.Scalar2MatrixConversions._
import org.fjn.matrix.MatrixExtensions._
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.fjn.matrix.Matrix
import scala.collection.immutable.IndexedSeq

class testQuasiNewton  extends AssertionsForJUnit{

  val mu = new Matrix[Double](5,1)
  mu(0,0)=0.05
  mu(1,0)=0.05
  mu(2,0)=0.05
  mu(3,0)=0.05
  mu(4,0)=0.01

  val auxC = 0.87
  val c = new Matrix[Double](5,5)
  c <=  s"1.0,$auxC,$auxC,$auxC,0.0;" +
    s"$auxC,1.0,$auxC,$auxC,0.0;"+
    s"$auxC,$auxC,1.0,$auxC,0.0;"+
    s"$auxC,$auxC,$auxC,1.0,0.0;"+
    "0.0,0.0,0.0,0.0,0.0"


  def pFitness(x:Matrix[Double])(phi:Double):Double={


    -(x.transpose * mu - 0.5 * phi * (x.transpose * c * x))(0,0)

  }

  @Test def testQN{
    val x0 = Seq(0.01,0.02,0.03,0.04,0.05).toMatrix
    val obj1 = QuasiNewtonSolver(x0,pFitness(_)(2),1e-6,5,QNUPDATE_TYPES.BFGSQNUPDATE,0.9)

    val xfinal1 = obj1.run(x0)


    val obj2 = QuasiNewtonSolver(x0,pFitness(_)(0.001),1e-6,5,QNUPDATE_TYPES.DFPQNUPDATE,0.9)

    val xfinal2 = obj2.run(x0)

    println("BFGSQNUPDATE result = %s r=%s".format(xfinal1._1,xfinal1._2))
    println("DFPQNUPDATE result = %s r=%s".format(xfinal2._1,xfinal2._2))

    val re1 = xfinal1._1.transpose* mu
    val sig1 =                        xfinal1._1.transpose* c * xfinal1._1

    val re2 = xfinal2._1.transpose* mu
    val sig2 =                        xfinal2._1.transpose* c * xfinal2._1
    println(s"portfolio x =${re1}. Sigam = $sig1")
    println(s"portfolio x =${re2}. Sigam = $sig2")


    org.junit.Assert.assertTrue(re1(0,0) < re2(0,0) && sig1(0,0) < sig2(0,0))
  }





}
