package org.fjn.interpolator.nurbs.instance

import org.fjn.interpolator.basis.ParameterVectorEqually
import org.fjn.matrix.Matrix

class Nurbs2DEqually(val qk: IndexedSeq[Matrix[Double]], val basisOrderForCoord: IndexedSeq[Int], val dim: IndexedSeq[Int], implicit val tolerance: Double = 1.0e-4)
    extends Nurbs2DBase with ParameterVectorEqually {

  val maxValX = qk.map(v => v(0, 0)).max
  val maxValY = qk.map(v => v(1, 0)).max

  val minValX = qk.map(v => v(0, 0)).min
  val minValY = qk.map(v => v(1, 0)).min

  override def getNormalizedCoord(x: Double, nCoord: Int): Double = {

    var maxVal = if (nCoord == 0) maxValX else maxValY
    var minVal = if (nCoord == 0) minValX else minValY

    val a = (x - minVal) / (maxVal - minVal)
    a

  }
}

