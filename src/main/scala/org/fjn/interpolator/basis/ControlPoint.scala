package org.fjn.interpolator.basis

import org.fjn.interpolator.common.MultiArrayView
import org.fjn.matrix.Matrix

/**
 * Control points define the grid where the NURBS must match the given function.
 */
trait ControlPoint {

  /**List of points conforming the n-Dimensional samples provided for interpolation */
  def qk: Seq[Matrix[Double]]

  /**sequence hosting the number of points per dimension. This sequence is used to compute */
  def dim: Seq[Int]

  /**Accessor to n-dimensional grid */
  lazy val viewQk = new MultiArrayView[Matrix[Double]](qk, dim)

}
