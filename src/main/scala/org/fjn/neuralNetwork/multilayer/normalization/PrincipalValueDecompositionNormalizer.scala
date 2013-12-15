package org.fjn.neuralNetwork.multilayer.normalization

import org.fjn.matrix.Matrix
import org.fjn.matrix.MatrixExtensions._
import org.fjn.neuralNetwork.multilayer.architecture.NetworkData
import org.fjn.neuralNetwork.reader.TrainingData
import scala.collection.immutable.IndexedSeq
import org.fjn.statistics.Estimators

/**
 * Normalizer which process input data as follows:
 * 1) Principal component analysis of the training set
 * 2) optional elimination of the redundant dimensions
 * 3) normalization of the results into the dynamic ranges of the trigger function coordinate-axis
 */
class PrincipalValueDecompositionNormalizer(val originalTrainingSet: Array[Matrix[Double]], val triggerFunc: (Double) => Double)
  extends Normalizer {

  val sampleMatrix = new Matrix[Double](originalTrainingSet.head.numberRows, originalTrainingSet.length)
  for {j <- 0 until originalTrainingSet.length
       i <- 0 until originalTrainingSet.head.numberRows} {
    sampleMatrix(i, j) = originalTrainingSet(j)(i, 0)
  }

  lazy val mean: Matrix[Double] = Estimators.mean(sampleMatrix)


  lazy val Cxx = {
    Estimators.covariance(sampleMatrix)
  }


  private val P = Cxx.eigVectors._2.transpose
  private val variance = {

    val aux1 =
      (for {
      i <- 0 until originalTrainingSet.head.numberRows
    }
    yield {
      val a = for (j <- 0 until originalTrainingSet.length) yield {
        originalTrainingSet(j)(i, 0)
      }
      (-a.min+a.max)
    }).toSeq

    val m = new Matrix[Double](aux1.length,aux1.length)

    for(i <- 0 until m.numberRows){
      m(i,i)= if(aux1(i)>0) 1.0/aux1(i) else 1.0
    }

    m

  }
  lazy private val Pinv = P.transpose


  private def intoPricipalComponentSpace(x: Matrix[Double]) = {
    val aux1 = (x - mean)
    val a = P * aux1
    a
  }


  import org.fjn.matrix.MatrixExtensions._

  def normalise(x: Matrix[Double]) = {

    val xn = intoPricipalComponentSpace(x)
    variance * xn


  }

  def deNormalise(x: Matrix[Double]) = {

    Pinv * x + mean

  }

  lazy val normalizedSamples: Array[Matrix[Double]] = originalTrainingSet.map(intoPricipalComponentSpace)

}
