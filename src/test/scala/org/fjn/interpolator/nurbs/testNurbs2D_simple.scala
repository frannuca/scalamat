package org.fjn.interpolator.nurbs

import instance.Nurbs2DEqually
import collection.immutable.IndexedSeq
import org.fjn.interpolator.common.MultiArrayView
import org.fjn.matrix.Matrix

object testNurbs2D_simple {

  def main(args: Array[String]) {

    val Ns = 4
    val R =
      for (
        j <- 0 until Ns;
        i <- 0 until Ns
      ) yield {

        val m = new Matrix[Double](2, 1)
        m(0, 0) = i.toDouble
        m(1, 0) = j.toDouble

        (m, (i + j).toDouble)
      }

    val vw = new MultiArrayView[(Matrix[Double], Double)](R, Seq(Ns, Ns))
    val _0_0 = vw(Seq(0, 0))
    val _0_3 = vw(Seq(0, 3))
    val _3_3 = vw(Seq(3, 3))
    val _3_0 = vw(Seq(3, 0))

    val qk = R.map(r => r._1)
    val z = R.map(r => r._2)

    val spline = new Nurbs2DEqually(qk, IndexedSeq(1, 1), IndexedSeq(Ns, Ns))

    spline.solve(z.toArray)

    val vv = spline.viewTQk(Seq(Ns - 1, 0))

    val err: IndexedSeq[Double] = for (
      i <- 0 until Ns;
      j <- 0 until Ns
    ) yield {

      val x0 = j.toDouble
      val y0 = i.toDouble
      val u = spline.getNormalizedCoord(x0, 0)
      val v = spline.getNormalizedCoord(y0, 1)
      val aprox = spline(u, v)
      val x = aprox(0, 0)
      val y = aprox(1, 0)
      val zc = aprox(2, 0)

      val dz = math.abs((i + j).toDouble - zc)
      dz
    }

    println("max error =" + err.max.toString)

  }

}
