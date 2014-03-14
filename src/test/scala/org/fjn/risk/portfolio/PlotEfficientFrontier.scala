package org.fjn.risk.portfolio
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.fjn.matrix.Matrix
import scala.collection.immutable.IndexedSeq


/**
 * Created by fran on 3/10/14.
 */
class PlotEfficientFrontier extends AssertionsForJUnit{

  @Test def  testPlot{

    val c = new Matrix[Double](4,4)
    c <=  "1.0,0.1,0.4,0.5;" +
          "0.1,1.0,0.7,0.4;"+
          "0.4,0.7,1.0,0.8;"+
          "0.5,0.4,0.8,1.0"


    val mu = new Matrix[Double](4,1)
    mu(0,0)=0.05
    mu(1,0)=0.06
    mu(2,0)=0.08
    mu(3,0)=0.06



    val r: IndexedSeq[(Double, Double)] = for( p <- 0 until 100000) yield {
      val x = new QuadraticUtilityWithSum1(c,mu,p).solve(null,null,0.0,1)._1

      ((x.transpose * c * x)(0,0),(x.transpose * mu)(0,0))

    }

    org.fjn.plot.PlotCurve(r.map(_._1*100).toSeq,r.map(_._2*100).toSeq)

    Thread.sleep(60000)

  }

}
