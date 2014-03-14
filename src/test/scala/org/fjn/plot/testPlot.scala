package org.fjn.plot

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import scala.concurrent.ExecutionContext

/**
 * Created by fran on 3/10/14.
 */
class testPlot extends AssertionsForJUnit{

  @Test def testPlot{


    import ExecutionContext.Implicits.global
    scala.concurrent.Future{

      org.fjn.plot.PlotCurve(Seq(),Seq())
    }

    Thread.sleep(50000)
  }

}
