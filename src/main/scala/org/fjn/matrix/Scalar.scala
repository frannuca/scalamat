package org.fjn.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 10/21/12
 * Time: 10:19 AM
 * To change this template use File | Settings | File Templates.
 */
case class Scalar[T1](value:T1)(implicit m2: Manifest[T1], implicit val m: Fractional[T1]) {


  def *(that:Matrix[T1]):Matrix[T1]={
    that * value
  }

  def +(that:Matrix[T1]):Matrix[T1]={
    that + value
  }

  def -(that:Matrix[T1]):Matrix[T1]={
    that - value
  }

  def /(that:Matrix[T1]):Matrix[T1]={
    val r = that.clone()
    val arr = r.getArray()
    for (i <- 0 until  arr.length){
      arr(i)= m.div(value,arr(i))
    }
    r
  }


}


object Scalar2MatrixConversions{

  implicit def double2Scalar(d:Double):Scalar[Double]={Scalar[Double](value=d)}
  implicit def float2Scalar(d:Float):Scalar[Float]={Scalar[Float](value=d)}
  implicit def int2Scalar(d:Int):Scalar[Double]={Scalar[Double](value=d)}
  implicit def complex2Scalar(d:Complex):Scalar[Complex]={Scalar[Complex](value=d)}

}