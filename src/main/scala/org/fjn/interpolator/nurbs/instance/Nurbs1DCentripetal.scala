package org.fjn.interpolator.nurbs.instance

import org.fjn.interpolator.basis.ParameterVectorCentripetal
import org.fjn.matrix.Matrix


class Nurbs1DCentripetal(val qk: IndexedSeq[Matrix[Double]], val basisOrderForCoord: IndexedSeq[Int], val tolerance: Double)
    extends Nurbs1DBase with ParameterVectorCentripetal {

}
