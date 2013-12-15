package org.fjn.neuralNetwork.multilayer.normalization

import org.specs2.mutable._
import org.fjn.matrix.Matrix
import org.fjn.matrix.MatrixExtensions._
import org.fjn.neuralNetwork.multilayer.activation.Sigmoidea

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 14.12.13
 * Time: 16:25
 * To change this template use File | Settings | File Templates.
 */
object normalizationTest extends App  {

  val originalTrainingSet= Array(Seq(0.01,0.01).toMatrix,Seq(20.0,20.0).toMatrix,Seq(14.0,15.0).toMatrix,Seq(24.0,22.0).toMatrix)
  val triggerFunc = new Sigmoidea()
  val normalizer = new PrincipalValueDecompositionNormalizer(originalTrainingSet,triggerFunc.trigger)



  originalTrainingSet.foreach(p => {
    println("from %s to %s".format(p,normalizer.normalise(p)))
  })




}
