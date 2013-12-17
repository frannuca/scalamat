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

  val originalTrainingSet= Array(Seq(-24.0,-24.0).toMatrix,Seq(-23.0,-23.0).toMatrix,Seq(0.00,0.00).toMatrix,Seq(24.0,24.0).toMatrix)
  val triggerFunc = new Sigmoidea()
  val normalizer = new PrincipalValueDecompositionNormalizer(originalTrainingSet,triggerFunc.trigger)



  originalTrainingSet.foreach(p => {
    println("from %s to %s -> back to %s ".format(p,normalizer.normalise(p),normalizer.deNormalise(normalizer.normalise(p))))
  })




}
