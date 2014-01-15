package org.fjn.optimization.pso

import org.fjn.optimization.evolutionary.pso.SwarmGA

object testSWARM extends App{


    val swarm = new SwarmGA(localfitness, 5, 10, List(-10d, -10d,-10d,-10d).toArray, List(10d, 10d,10d,10d).toArray, 0.00, 0.5, 0.25, 0.25)

    for (i <- 0 until 50) {
      swarm.next();
    }

    println(swarm.population.gBest.toString())
    println(swarm.population.gBest.bestFitnessValueNow)
    swarm.population.gBest.bestFitnessValueNow<1e-5


  def localfitness(x: Array[Double]): Double = {

    val p = 5
    ((x (0) - p) * (x (0) - p ) + (x (1) - p) * (x (1) - p) + (x (2) + p) * (x (2) + p) + (x (3) - p) * (x (3) - p) );

  }
}
