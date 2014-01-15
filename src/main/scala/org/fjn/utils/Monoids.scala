package org.fjn.utils

trait Monoid[A]{
  def mappend(a1: A, a2: A): A
  def mzero: A
}




object Monoids{
  implicit val intMonoid = new Monoid[Int] {
    def mappend(a1: Int, a2: Int): Int = a1 + a2
    def mzero:Int = 0
  }
  implicit val doubleMonoid =  new Monoid[Double] {
    def mappend(a1: Double, a2: Double): Double = a1 + a2
    def mzero:Double = 0
  }
  implicit val strMonoid =  new Monoid[String] {
    def mappend(a1: String, a2: String): String = a1 + a2
    def mzero:String = ""
  }
}

object Operations{
  def sum[A:Monoid](x:List[A]):A={
    val m = implicitly[Monoid[A]]
    x.foldLeft(m.mzero)(m.mappend)
  }
}


object testMonoid extends App{

  import  Monoids._
  val lst = List(1.0,2.0,3.0)
  val r = Operations.sum(lst)

  val lst2 = List("a","b","c")
  val r2= Operations.sum(lst2)
  println(r+"\n"+r2)


  import scalaz._
  import std.option._, std.list._


}