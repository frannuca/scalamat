package org.fjn.interpolator.nurbs.instance

import org.fjn.interpolator.basis.ParameterVectorChord
import org.fjn.matrix.Matrix


class Nurbs1DChord(val qk: Seq[Matrix[Double]], val basisOrderForCoord: Seq[Int], val tolerance: Double)
    extends Nurbs1DBase with ParameterVectorChord {

}
