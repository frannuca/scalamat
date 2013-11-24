package org.fjn.interpolator.nurbs.instance

import org.fjn.interpolator.basis.{ ParameterVectorCentripetal, ControlPoint }
import org.fjn.matrix.Matrix

class Nurbs2DCentripetal(val qk: IndexedSeq[Matrix[Double]], val basisOrderForCoord: IndexedSeq[Int], val dim: IndexedSeq[Int], implicit val tolerance: Double = 1.0e-4)
    extends ControlPoint with ParameterVectorCentripetal with Nurbs2DBase {

  println("Nurbs2DCentripetal")

}
