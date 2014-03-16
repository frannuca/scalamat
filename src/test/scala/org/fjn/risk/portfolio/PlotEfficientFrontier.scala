//package org.fjn.risk.portfolio
//import org.scalatest.junit.AssertionsForJUnit
//import org.junit.Test
//import org.fjn.matrix.Matrix
//import scala.collection.immutable.IndexedSeq
//
//
///**
// * Created by fran on 3/10/14.
// */
//class PlotEfficientFrontier extends AssertionsForJUnit{
//
//  @Test def  testPlot{
//
//    val auxC = 0.5
//    val c = new Matrix[Double](5,5)
//    c <=  s"1.0,$auxC,$auxC,$auxC,0.0;" +
//          s"$auxC,1.0,$auxC,$auxC,0.0;"+
//          s"$auxC,$auxC,1.0,$auxC,0.0;"+
//          s"$auxC,$auxC,$auxC,1.0,0.0;"+
//          "0.0,0.0,0.0,0.0,0.0"
//
//
//    val mu = new Matrix[Double](5,1)
//    mu(0,0)=0.05
//    mu(1,0)=0.05
//    mu(2,0)=0.05
//    mu(3,0)=0.05
//    mu(4,0)=0.01
//
//
//
//
//    val r: IndexedSeq[(Double, Double)] = for( p <- 1 until 10) yield {
//      val x = new QuadraticUtilityWithFreeRiskAsset(c,mu,p/2.0).solve(null,null,0.0,1)._1
//
//      ((x.transpose * c * x)(0,0),(x.transpose * mu)(0,0))
//
//    }
//
//    org.fjn.plot.PlotCurve(r.map(_._1*100).toSeq,r.map(_._2*100).toSeq)
//
//    Thread.sleep(60000)
//
//  }
//
//}
