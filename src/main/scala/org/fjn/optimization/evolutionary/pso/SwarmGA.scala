package org.fjn.optimization.evolutionary.pso

import commons.{SwarmPop}
import org.fjn.optimization.evolutionary.genetic.commons.tAlgorithm
import org.fjn.matrix.Matrix


/**
* User: fran
* Date: 1/18/12
* Time: 7:14 PM
*/

/**
 *
 * @param pFitness
 * @param numberOfCluster
 * @param numberOfParticlePerCluster
 * @param minLimit
 * @param maxLimit
 * @param velocityMomentum
 * @param towardsGlobalAcceleration
 * @param towardsClusterAcceleration
 * @param towardsBestParticleAcceleration
 */
case class SwarmGA(pFitness:(Array[Double]) => Double,numberOfCluster:Int,
                      numberOfParticlePerCluster:Int,
                      minLimit:Array[Double],maxLimit:Array[Double],
                      velocityMomentum:Double, 
                      towardsGlobalAcceleration:Double,
                      towardsClusterAcceleration:Double, 
                      towardsBestParticleAcceleration:Double
                    )
                     extends tAlgorithm[Double]{


  
  
  require(minLimit.length == maxLimit.length && minLimit.length>0)

  val particleDimension = minLimit.length

  var population:SwarmPop=init()



  private def init():SwarmPop={

     new SwarmPop(numberOfCluster,numberOfParticlePerCluster,minLimit,maxLimit,pFitnessTranformation,
       towardsGlobalAcceleration,towardsClusterAcceleration,towardsBestParticleAcceleration,velocityMomentum)

  }
  def pFitnessTranformation(x:Matrix[Double]):Double =
  {
    pFitness(x.getArray())
  };




     /**
     * Evolution of the tAlgorithm is performed through calls to next
     */
     def next():Boolean={
       population.algorithmComposition(true)
     }







}

object testSWARM extends App{


  val swarm = new SwarmGA(localfitness, 10, 15, List(-500d, -500d,-500d,-500d).toArray, List(500d, 500d,500d,500d).toArray, 0.4,0.33, 0.33, 0.33)

  var cont = false
  for (i <- 0 until 100) {
    swarm.next();
    val index = i
    println(s"BEGIN of iteration ${index}")
    println(s"${swarm.population.gBest.toString()}")
    println(s"best fitness = ${swarm.population.gBest.bestFitnessValueNow}")
    println(s"velocity = ${swarm.population.gBest.velocity}")
    println("END iteration ${index}")
    if(!cont){

      readLine() match{
        case "ok" => cont=true
        case _=>
      }

    }

  }


  swarm.population.gBest.bestFitnessValueNow<1e-5


  def localfitness(x: Array[Double]): Double = {

    val p = 5
    math.abs((x (0) - p))  + math.abs((x (1) - p))  +  math.abs((x (2) + p)) +  math.abs((x (3) - p) );

  }
}
