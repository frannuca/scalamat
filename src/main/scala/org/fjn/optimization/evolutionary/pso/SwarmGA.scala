package org.fjn.optimization.evolutionary.pso

import org.fjn.optimization.evolutionary.pso.commons.{Particle, SwarmPop}
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
case class SwarmGA(pFitness: (Array[Double]) => Double, numberOfCluster: Int,
                   numberOfParticlePerCluster: Int,
                   minLimit: Array[Double], maxLimit: Array[Double],
                   velocityMomentum: Double,
                   towardsGlobalAcceleration: Double,
                   towardsClusterAcceleration: Double,
                   towardsBestParticleAcceleration: Double
                    )
  extends tAlgorithm[Double] {


  require(minLimit.length == maxLimit.length && minLimit.length > 0)

  val particleDimension = minLimit.length

  var population: SwarmPop = init()


  private def init(): SwarmPop = {

    new SwarmPop(numberOfCluster, numberOfParticlePerCluster, minLimit, maxLimit, pFitnessTranformation,
      towardsGlobalAcceleration, towardsClusterAcceleration, towardsBestParticleAcceleration, velocityMomentum)

  }

  def pFitnessTranformation(x: Matrix[Double]): Double = {
    pFitness(x.getArray())
  };


  /**
   * Evolution of the tAlgorithm is performed through calls to next
   */
  def next(): Boolean = {
    population.algorithmComposition(true)
  }


}

object testSWARM extends App {
  val NPAR = 15;

  val PSeq = (0 until NPAR)

  def runOpt(localfitness: (Array[Double] => Double), min: Double, max: Double) = {

    val swarm = new SwarmGA(localfitness, 5, 30, PSeq.map(_ => min).toArray, PSeq.map(_ => max).toArray.toArray, 0.88, 0.75, 0.5, 0.33)

    var cont = false
    for (i <- 0 until 1000) {
      swarm.next();
      val index = i
      // println(s"BEGIN of iteration ${index} --> ${swarm.population.gBest.bestFitnessValueNow}")
      //println(s"${swarm.population.gBest.toString()}")
      if(i%10 == 0)
      println(s"best fitness ${index} = ${swarm.population.gBest.pNow}")
      //println(s"velocity = ${swarm.population.gBest.velocity}")
      //println(s"END iteration ${index}")
      /*if(!cont){

        readLine() match{
          case "ok" => cont=true
          case _=>
        }

      } */

    }

    //println(s"${swarm.population.gBest.toString()}")

    swarm.population.gBest
  }

  import org.fjn.matrix.MatrixExtensions._

  def presentResult(name: String, f:() =>  Particle) = {
    println()
    println(s"starting "+name)
    val s1=f()

    println("fitness1=" + s1.bestFitnessValueNow)
    println("fitness2=" + s1.pNow)

    println(s"finishing "+name)
  }


  presentResult("modules function(5.0,...)",()=> runOpt(localfitness1, -100, 100))

  readLine()
  presentResult("RosenBrock r=(1.0,....)  ", () => runOpt(x =>{
    x.indices.tail.foldLeft(0.0)((acc,i)=>
     acc + {100.0*math.pow(x(i)-x(i-1)*x(i-1),2.0)+math.pow((x(i-1)-1),2)}
     )
  },-30.0,30.0))

  readLine()

  presentResult("sphere r=(0,0,...0)", ()=>runOpt(x => x.foldLeft(0.0)((acc, x) => acc + x * x), -500, 500))
  readLine()

  presentResult("Schwefel problem r=(0,0,...0)", () => runOpt(x => x.foldLeft(0.0)((acc, x) => acc + math.abs(x)) + x.foldLeft(1.0)((acc, x) => acc * math.abs(x)), -10, 10))


  def localfitness1(x: Array[Double]): Double = {


    math.abs(PSeq.foldLeft(0.0)((acc, i) => math.abs(x(i) - 5) + acc))

  }

}
