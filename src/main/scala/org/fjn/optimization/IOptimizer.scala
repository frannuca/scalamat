package org.fjn.optimization

import scala.collection.immutable.IndexedSeq
import org.fjn.matrix.Matrix

object MatrixType {type DMatrix = org.fjn.matrix.Matrix[Double]}

trait IOptimizer {


  import MatrixType._
  def solve(x0:DMatrix,functor:(DMatrix)=>Double,tolerance:Double,maxIter:Int):(Matrix[Double],Double)

}
