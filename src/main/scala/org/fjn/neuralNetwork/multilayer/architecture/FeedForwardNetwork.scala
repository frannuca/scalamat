package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.neuralNetwork.multilayer.algorithm.BackPropagation
import org.fjn.neuralNetwork.multilayer.normalization.PrincipalValueDecompositionNormalizer


class FeedForwardNetwork(nnDatax:NetworkData,lr0x:Double,momentum0x:Double)
  extends PrincipalValueDecompositionNormalizer(nnDatax.dataSet.map(s => s.input).toArray,nnDatax.activationFunction.trigger)
  with Network
  with BackPropagation
   {
      val nnData: org.fjn.neuralNetwork.multilayer.architecture.NetworkData =     nnDatax
      val originalTrainingSet: Array[org.fjn.matrix.Matrix[Double]] =   nnDatax.dataSet.map(_.input).toArray
      val triggerFunc: Double => Double = nnDatax.activationFunction.trigger
      val lr0: Double = lr0x
      val momentum0: Double = momentum0x
}
