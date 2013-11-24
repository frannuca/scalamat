package org.fjn.interpolator.nurbs.instance

import org.fjn.interpolator.basis.ParameterVectorChord
import org.fjn.matrix.Matrix

class Nurbs2DChord(val qk: Seq[Matrix[Double]], val basisOrderForCoord: Seq[Int], val dim: Seq[Int], implicit val tolerance: Double = 1.0e-2)
    extends ParameterVectorChord with Nurbs2DBase {

  println("Nurbs2DChord")

}
