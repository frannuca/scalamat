package org.fjn.plot

import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.ui.ApplicationFrame
import org.jfree.ui.RefineryUtilities


private   class XYSeriesDemo(title:String,x:Seq[Double],y:Seq[Double]) extends ApplicationFrame(title) {



  val series = new XYSeries("Random Data");
  (x zip y).foreach(w =>{
    series.add(w._1, w._2);
  })

  val data = new XYSeriesCollection(series);
  val chart = ChartFactory.createXYLineChart(
    "XY Series Demo",
    "X",
    "Y",
    data,
    PlotOrientation.VERTICAL,
    true,
    true,
    false
  )

  data.setAutoWidth(true)

  val chartPanel = new ChartPanel(chart)
  chartPanel.setPreferredSize(new java.awt.Dimension(500, 270));
  setContentPane(chartPanel);

}

object PlotCurve {


  def apply(x:Seq[Double],y:Seq[Double]){
    val demo = new XYSeriesDemo("XY Series Demo",x,y);
    demo.pack();
    RefineryUtilities.centerFrameOnScreen(demo);
    demo.setVisible(true);
  }
}

