package org.fjn.interpolator.nurbs.solver

import org.fjn.interpolator.basis.{ ParameterVector, BasisFunctionOrder, Basis, ControlPoint }
import org.fjn.matrix.Matrix


/**
 * Created by fjn
 * User: fran
 * Date: 5/8/12
 * Time: 7:41 AM
 * To change this template use File | Settings | File Templates.
 */

trait Solver1D {

  self: Basis with ParameterVector with ControlPoint with BasisFunctionOrder =>


  val init = self.tqk
  val numberOfSamples: Int = self.tqk.length
  var pk: Matrix[Double] = new Matrix[Double](1, 1)
  val weights: Array[Double] = new Array[Double](self.qk.length)

  def solve(z: Array[Double]): Boolean = {

    val listOfMatrix =
      for (k <- 0 until dim.length) yield {
        val qMatrix = new Matrix[Double](numberOfSamples, numberOfSamples)
        for (i <- 0 until numberOfSamples) {
          for (j <- 0 until numberOfSamples) {
            val auxU = tqk(i)(k, 0)
            val vv = NBasis(j, basisOrderForCoord(k), k)(auxU)
            qMatrix(i, j) = vv
          }

        }
        qMatrix

      }

    var rightM = new Matrix[Double](numberOfSamples, dim.length + 1)
    for (i <- 0 until numberOfSamples) {
      for (j <- 0 until dim.length)
        rightM(i, j) = qk(i)(j, 0)

      rightM(i, dim.length) = z(i)
    }

    var mSol = new Matrix[Double](numberOfSamples, dim.length + 1)
    //computing the contol points:
    listOfMatrix.foreach(_.invert)
    for (m <- listOfMatrix) {

      rightM = m * rightM
    }

    pk = rightM

    true
  }

}

