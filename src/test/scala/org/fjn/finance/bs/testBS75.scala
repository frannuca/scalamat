//package org.fjn.finance.bs
//
//import org.fjn.matrix.Matrix
//import org.fjn.optimization.gradient.nonLineal.{GradientDescent, DFPQNUpdate, QuasiNewton}
//import org.apache.commons.math3.optimization.univariate.{UnivariatePointValuePair, BrentOptimizer}
//import org.apache.commons.math3.analysis.UnivariateFunction
//import org.apache.commons.math3.optimization.GoalType
//import org.jfree.data.xy.{XYSeriesCollection, XYSeries}
//import org.jfree.chart.{ChartUtilities, ChartFactory, JFreeChart}
//import sun.net.www.content.text.PlainTextInputStream
//import org.jfree.chart.plot.PlotOrientation
//import java.io.File
//
//object testBS75 extends App{
//  val bs = (sigma:Double) =>{
//
//    val a = math.log(BS76(111,1,2,100,sigma,0.01)) - math.log(17.78)
//    a*a
//
//  }
//
//  implicit def toUniFunc(f:(Double)=>Double)={
//    new UnivariateFunction {
//      def value(x: Double): Double = f(x)
//    }
//  }
//  val b = new BrentOptimizer(0.01,1e-5)
//  val sigma: UnivariatePointValuePair = b.optimize(10000,bs,GoalType.MINIMIZE,0.2,0.3)
//
//
//  val pFitness = (v:Matrix[Double]) => bs(v(0,0))
//
//
//
//  import org.fjn.matrix.MatrixExtensions._
//  val gd = new GradientDescent(pFitness,Seq(0.1).toMatrix)
//  val rgq= gd ++ 10
//
//  val qnewton1 = new {
//    var alpha = 0.1
//    val pFunc = pFitness
//    val tolerance = Seq(1e-6)
//    val x0 = new Matrix[Double](1,1)<=Seq(0.1)
//  } with QuasiNewton with DFPQNUpdate
//
//
//  val r = qnewton1++100
//
////  val cc = new GradientDescent(bs,1e-1,1e-3,0.1)
////  val r = cc++1000
//  println("price=%s".format(r))
//
//  val series = new XYSeries("BS76")
//  for(i <- 1 until 10)  {
//    val s = 1.0/10*i
//    println("DFPQN volat = %s error=%s".format(s,bs(s))  )
//    series.add(s,bs(s))
//  }
//  val collection = new XYSeriesCollection()
//  collection.addSeries(series)
//
//  val chart = ChartFactory.createXYLineChart(
//  "BS",
//  "vol",
//  "npv",
//  collection,
//  PlotOrientation.VERTICAL,
//  true,
//  true,
//  false
//  )
//  try{
//    ChartUtilities.saveChartAsJPEG(new File("C:\\Users\\fran\\Downloads\\BS2.jpeg"), chart, 1000, 600);
//  }
//  catch{
//    case e =>    println(e.getMessage)
//
//  }
//  println("DFPQN volat = %s error=%s".format(sigma.getPoint,bs(sigma.getValue)))
//  println("price=%s".format( BS76(111,1,2,100,0.25,0.01)))
//
//
//}
