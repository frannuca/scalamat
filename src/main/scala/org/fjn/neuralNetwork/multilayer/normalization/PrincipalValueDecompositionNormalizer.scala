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



  //Building the sample matrix
  private val sampleMatrix = {
    val aux = new Matrix[Double](originalTrainingSet.head.numberRows, originalTrainingSet.length)
    for {j <- 0 until originalTrainingSet.length
         i <- 0 until originalTrainingSet.head.numberRows} {
      aux(i, j) = originalTrainingSet(j)(i, 0)
    }

    aux
  }

  /**
   * Mean of the samples
   */
  private val mean: Matrix[Double] = Estimators.mean(sampleMatrix)

  /**
   * Covariance matrix
   */
  private val Cxx = {
    Estimators.covariance(sampleMatrix)
  }


  /**
   * eigen vectors for the covariance matrix...
   */
  private val P = Cxx.eigVectors._2.transpose

  private val Pinv = P.transpose


  /**
   * transforms a given input into the PCA
   * @param x
   * @return
   */
  private def intoPCA(x:Matrix[Double])={
    P * (x - mean)
  }

  private def fromPCA(x:Matrix[Double])={
    Pinv * x + mean
  }


  val normalizedSamples = {
    val aux0 = originalTrainingSet.map(intoPCA)
    val aux = new Matrix[Double](aux0.head.numberRows, aux0.length)
    for {j <- 0 until aux0.length
         i <- 0 until aux0.head.numberRows} {
      aux(i, j) = aux0(j)(i, 0)
    }

    aux
  }

  val scaling = {

    val subCols= (0 until normalizedSamples.numberCols).toSeq

    val factors = (for(i <- 0 until normalizedSamples.numberRows) yield
    {
      val s = normalizedSamples.sub(Seq(i),subCols).getArray()
      math.abs(s.max) max  math.abs(s.min)
    }).toSeq

    val m = new Matrix[Double](factors.length,factors.length)
    for( i <- 0  until m.numberRows){
      m(i,i)= if(factors(i)!=0) 1.0/factors(i) else 1.0
    }
    m
  }

  val inv_scaling = {


    val m = scaling.clone()
    for( i <- 0  until m.numberRows){
      m(i,i)= 1.0/m(i,i)
    }
    m
  }


  import org.fjn.matrix.MatrixExtensions._

  def normalise(x: Matrix[Double]) = {
    val y = (intoPCA(x))
    val z=scaling *y
    z
  }

  def deNormalise(x: Matrix[Double]) = {

    fromPCA(inv_scaling * x )

  }



}
