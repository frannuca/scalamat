package org.fjn.optimization.genetic

import org.fjn.optimization.evolutionary.genetic.TreeGA

object TreeGA_Test  {




    def main(args:Array[String]) {
      val popSize = 100;
      val elitismFactor = 0.35;
      val nBits = Array (18, 18, 18, 18);
      val minLevel = Array (- 100d, -100d, -100d, - 100d)
      val maxLevel = Array (10d, 10d, 10d, 10d)
      def pFunc(x: Array[Double]): Double = {


        val p = 2.5
      1.0 / ((x (0) - p) * (x (0) - p ) + (x (1) - p) * (x (1) - p) + (x (2) + p) * (x (2) + p) + (x (3) - p) * (x (3) - p) );

    }

      //r
      //}


      val mutationPerc = 0.01;

      val talg = new TreeGA (popSize, elitismFactor, nBits, minLevel, maxLevel, pFunc, mutationPerc);

      for (i <- 0 until 100) {
      talg += 3;
      val res = talg.storage.getBest (1);
      print ("numberOfIterations=" + talg.iterations.toString + " ---> ")
      for (k <- 0.until (res.size) ) {
      res (k).chr.getValue ().foreach (x => {
      print (x + ";")
    });
      println ("");
    }
    }

      val resArray = Array (- 9, 9, - 9, 9);

      (talg.storage.getBest (1) (0).chr.getValue ().toList, resArray.toList).zipped.forall ((x, y) => (scala.math.abs (x - y) <= 0.5) )

    }

  }

