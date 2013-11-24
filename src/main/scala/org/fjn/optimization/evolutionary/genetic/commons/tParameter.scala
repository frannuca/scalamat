package org.fjn.optimization.evolutionary.genetic.commons

/**
 * User: fran
 * Date: 1/23/12
 * Time: 1:53 PM
 */

trait tParameter {

 def setValue(v:Double)
 def getValue():Double
 val minLevel: Double
 val maxLevel: Double

}