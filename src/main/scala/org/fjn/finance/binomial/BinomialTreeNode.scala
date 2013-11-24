package org.fjn.finance.binomial

import scala.collection.mutable.ListBuffer

abstract class INode{
  var f:Double=0.0
}
case class Node(spot:Double) extends INode

case class Terminal() extends INode



object BinomialTreeNode{

  /**
   * Computes the price of a european vanilla option
   * @param S0
   * @param n
   * @param u
   * @param dt
   * @param r
   * @return
   */
  def priceEuropean(S0:Double,n:Int,vol:Double,dt:Double,r:Double)(implicit payoff:Double=>Double):Double={

    val u = 1+vol/100.0
    val d = 1-vol/100.0
     val tree = new ListBuffer[Array[Node]]
    //initializing the first node (with spot=S0)
     tree += Array(Node(S0))

     for{i<- 1 until n}{
       tree += ( (for(j <- 0 to tree(i-1).length-1) yield{
         Node(tree(i-1)(j).spot* (1-u))
       }).toArray ++ Array(Node(tree(i-1).last.spot*u)))
    }

    tree.last.foreach(node => node.f=payoff(node.spot))



    val p = (math.exp(r*dt)-d)/(u-d)


    for{i<- n-1 to 1 by -1 }{
      for(j<- 0 until tree(i).length-1){
        tree(i-1)(j).f = p*tree(i)(j+1).f+(1-p)*tree(i)(j).f
      }

    }


    tree.head.head.f

  }
}



object testprice extends App{
  val k = 109
  for( i<- 0 until 50){
    val npv1 = BinomialTreeNode.priceEuropean(100,10,10,2.0/365.0,i.toDouble/100.0)((spot:Double)=> math.max(0.0,spot-k))
    println ("vol = %s,npv=%s".format(i,npv1))
  }




}