package org.fjn.interpolator.nurbs.solver

import org.fjn.interpolator.common.MultiArrayView
import collection.immutable.IndexedSeq
import org.fjn.matrix.Matrix

//import org.fjn.matrix.Matrix
import org.fjn.interpolator.basis.{ ParameterVector, BasisFunctionOrder, Basis, ControlPoint }

/**
 * Control points are in the original space.
 * Parameter vector points are in the normalized space.
 * The knots are the points defined by the basis curves.
 * {{{
 *   +------------------+
 *   | Control points   |  +----x--------x---------x---------x---------x-----+
 *   +------------------+
 *          |                   +     +--+   +-----+         +         +
 *          |                   |     |      |    +----------+         |
 *          |                   |     |      |    |     +--------------+
 *          v                   v     v      v    v     v
 *   +------------------+
 *   | Parameter vector |  +----x-----x-----x-----x-----x--------------------+
 *   +------------------+
 *          |
 *          |
 *          |
 *          v
 *   +------------------+
 *   | Knots            |  +----x--x--x--x--x--x--x--x--x--------------------+
 *   +------------------+
 * }}}
 */
trait Solver2D {
  self: Basis with ParameterVector with ControlPoint with BasisFunctionOrder =>

  val pointDimension = 3 //(x,y,z)

  self.qk
  val weights: Array[Double] = new Array[Double](self.qk.length)
  var pk: Seq[Matrix[Double]] = Seq()

  def SolveOnU(z: Seq[Double], viewer_norm: MultiArrayView[Matrix[Double]],
    viewer_original: MultiArrayView[Matrix[Double]], viewerZ: MultiArrayView[Double]): IndexedSeq[Matrix[Double]] = {
    //Preparing the matrix for constant y-slices:
    var qXMatrix = new Matrix[Double](dim(0), dim(0))
    for (i <- 0 until dim(0)) {
      val uk = parameterKnots(0)(i)
      for (j <- 0 until dim(0)) {

        val vv = NBasis(j, basisOrderForCoord(0), 0)(uk)
        qXMatrix(i, j) = vv
      }
    }
    qXMatrix.invert

    val sampleSize = 3 //(x,y,z)

    //Solving linear systems for y index (x,0),(x,1),....,(x,m-1) with x 0 .. n-1
    //var Rl: Seq[Matrix[Double]] =
    val futuresOnSystemSolver: Seq[() => Matrix[Double]] =
      for (l <- 0 until dim(1)) //we have dim(1) dim(1) points in y-direction, which are slices now
      yield {

        //Building linear system for the kth slice:

        //Basis function matrix:

        //sample vector(right side of the system):

        val f: () => Matrix[Double] = () => {
          val rightM = new Matrix[Double](dim(0), sampleSize)
          for (i <- 0 until dim(0)) {
            val auxPos = Seq(i, l)
            val posq = viewer_original(auxPos)
            for (j <- 0 until sampleSize - 1) {

              rightM(i, j) = posq(j, 0)
            }

            val auxZ = viewerZ(auxPos)
            rightM(i, sampleSize - 1) = auxZ
          }

          val auxVal = qXMatrix * rightM
          auxVal

        }
        f
      }

    val result = futuresOnSystemSolver.par.map(f => f())
    result.seq.toIndexedSeq
  }

  def solve(z: Seq[Double]) = {

    val viewerZ = new MultiArrayView[Double](z, dim)

    val Rl: IndexedSeq[Matrix[Double]] = SolveOnU(z, viewTQk, viewQk, viewerZ)

    val qXMatrix = new Matrix[Double](dim(1), dim(1))
    for (i <- 0 until dim(1)) {
      val vk = parameterKnots(1)(i)
      for (j <- 0 until dim(1)) {
        val vv = NBasis(j, basisOrderForCoord(1), 1)(vk)
        qXMatrix(i, j) = vv
      }
    }

    val qXMatrixInv = qXMatrix.clone()
    qXMatrixInv.invert

    val futuresOnSystemSolver: Seq[() => Matrix[Double]] =
      for (k <- 0 until dim(0)) //we have dim(1) dim(1) points in y-direction, which are slices now
      yield {

        //sample vector(right side of the system):
        () =>
          {
            val rightM = new Matrix[Double](dim(1), pointDimension)
            for (i <- 0 until dim(1)) {
              for (j <- 0 until pointDimension) {
                val rl: Matrix[Double] = Rl(i)
                rightM(i, j) = rl(k, j)
              }
            }

            qXMatrixInv * rightM
          }

      }

    pk = futuresOnSystemSolver.par.map(f => f()).seq
  }

}