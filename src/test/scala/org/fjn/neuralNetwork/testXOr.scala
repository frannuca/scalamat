package org.fjn.neuralNetwork


import multilayer.architecture.{NetworkData, FeedForwardNetwork}

import org.fjn.neuralNetwork.multilayer.activation.{AsymetricSigmoidea, Sigmoidea, ActivationFunction}

import org.fjn.neuralNetwork.reader.{TrainingData}
import org.fjn.neuralNetwork.generator.XORGenerator


object NN_test extends  App{


  {

    //val filepath = getClass().getResource("/"+ "dowjones.csv").getFile;






    val data = new NetworkData(
      layerDimensions = Seq(2,6,2,1),
      activationFunction = new Sigmoidea(),
      dataSet = XORGenerator()
    )

    val ffnn = new FeedForwardNetwork(
      nnDatax =data,
       lr0x = .1,
      momentum0x = 0.007
    )


    val err = ffnn.solve(20000)

    println("total Err"+err.toString)

    XORGenerator().foreach(d => println("in (%s), out = %s, eou=%s".format(d.input.toString,ffnn(d.input).toString,d.output.toString)))
    true
  }

}