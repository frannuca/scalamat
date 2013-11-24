package org.fjn.optimization.evolutionary.genetic.commons

object populationType extends Enumeration {
        type populationType = Value
        val CONTINUOUS,DISCRETE = Value
      }

import populationType._
import collection.mutable.ListBuffer


/**
 * defines the minimum components needed to interact with the setChromosomeI of trials used during
 * an optimization process
 */
case class popPair[T](key:T,v:Double) extends Ordered[popPair[T]]{

  var chr:T = key;
  var value:Double = v;

  def compare(y: popPair[T]):Int ={
    value.compare(y.value)
  }
}




trait tPopulation[T] {

   def init():Unit;
   def getPopulationType:populationType;

   var population:Array[popPair[T]];


   def totalParameterLength:Int;
    /**
   * returns the numberOfSamples best vector with their corresponding fitness as an array of 2-tuple
   */
  def getBest(numberOfSamples:Int):Array[popPair[T]]={


      var filteredPop = population.clone();

      (0 until math.min(numberOfSamples,population.length)).map(n =>{
        if(filteredPop.isEmpty) None
        else{
          val aux = filteredPop.max
          filteredPop = filteredPop.filter(y => y != aux)
          Some(aux)
        }

      }).flatten.toArray
  }
   def getWorst():Int={

     population.indexOf(population.min)

  }

  override def toString():String ={
    val res = new StringBuilder()
    population.foreach(x => res.append( x.value.toString()+" ->"+ x.chr.toString()+";"));

    res.append("\n")
    res.toString()
  }
}