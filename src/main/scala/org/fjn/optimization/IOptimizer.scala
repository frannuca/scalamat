package org.fjn.optimization

import scala.collection.immutable.IndexedSeq
import org.fjn.matrix.Matrix

object MatrixType {type DMatrix = org.fjn.matrix.Matrix[Double]}

abstract class IOptimizer(function:(MatrixType.DMatrix => Double)) {


  import MatrixType._

  private var x0:Option[DMatrix]=None
  private var numberOfIterations:Int = 0
  private var tolerance:Option[Double] = None

  def init(initialGuess:DMatrix,tol:Double)        {
    x0 = Some(initialGuess)
    numberOfIterations = 0
    tolerance = Some(tol)
  }


  def ++(nIterations:Int): Seq[(DMatrix, Double)]
}
