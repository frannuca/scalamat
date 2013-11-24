package org.fjn.matrix

import java.io.{ObjectInputStream, FileInputStream, ObjectOutputStream, FileOutputStream}


object testOperators{

  def testProduct(rowMajor:Boolean){

    val m1  = new Matrix[Double](3,2,rowMajor)
    val m2 = new Matrix[Double](2,3,rowMajor)
    val m3 = new Matrix[Double](3,3,rowMajor)


    m1.random
    m2.random
    m3.random

    val r = m1 * m2 + m3 - m3 * 2.0

    val m1b= m1.toJama
    val m2b= m2.toJama
    val m3b= m3.toJama


    var rb = m1b.times(m2b)
    rb = rb.plus(m3b)
    rb = rb.minus(m3b.times(2.0))
    val rnew = new Matrix[Double](3,3,rowMajor)
    rnew.fromJama(rb)

    require(r == rnew)
  }

  def testToFromJama(rowMajor:Boolean){
    val m1  = new Matrix[Double](3,2,rowMajor)
    m1.random
    val jm1 = m1.toJama

    val m1copy = m1.clone()
    m1.fromJama(jm1)

    require(m1 == m1copy)
  }


  def testInversion(rowMajor:Boolean){
    val mInv = new Matrix[Double](3,3,rowMajor)
    mInv.random
    val m2 = mInv.clone()
    mInv.invert

    val r = m2 * mInv

    val I = new Matrix[Double](3,3,rowMajor)
    I.eye

    require(r == I)

  }

  def testTranspose(rowMajor:Boolean){
    val m1 = new Matrix[Double](3,3,rowMajor)
    m1.random


    val m1t = new Matrix[Double](m1.numberCols,m1.numberRows)

    for( i <- 0 until m1.numberRows;
         j <- 0 until m1.numberCols){
      m1t.set(i,j,m1(j,i))
    }


    require(m1.transpose == m1t)
  }

  def testCholesky(rowMajor:Boolean){
    val m1 = new Matrix[Double](3,3,rowMajor)
    m1.set(0,0,9.25)
    m1.set(0,1,3.5)
    m1.set(0,2,-2.5)
    m1.set(1,1,18.3)
    m1.set(1,2,5.33)
    m1.set(2,2,14.75)


    for (i <- 0 until m1.numberRows;
         j <- i until m1.numberCols){
      m1.set(j,i,m1(i,j))
    }

    val mm = m1.eigVectors

    val mCholesky = m1.cholesky
    val r =  mCholesky * mCholesky.transpose

    require(r == m1)


  }

  def testEigen(rowMajor:Boolean){
    val m1 = new Matrix[Double](3,3,rowMajor)
    m1.set(0,0,9.25)
    m1.set(0,1,3.5)
    m1.set(0,2,-2.5)
    m1.set(1,1,18.3)
    m1.set(1,2,5.33)
    m1.set(2,2,14.75)


    for (i <- 0 until m1.numberRows;
         j <- i until m1.numberCols){
      m1.set(j,i,m1(i,j))
    }

    val mm = m1.eigVectors

    val eigVec = mm._2
    val eigVal = mm._1

    val x1 = m1 * eigVec
    val x2 = eigVec * eigVal

    require(x1 == x2)



  }


  def testSubMatrix(rowMajor:Boolean){
    val m1 = new Matrix[Double](3,3,rowMajor)
    m1.set(0,0,1.0)
    m1.set(0,1,2.0)
    m1.set(1,0,4.0)

    val m2 = new Matrix[Double](2,1,rowMajor)
    m2.set(0,0,1.0)
    m2.set(1,0,4.0)



    val sm1 = m1.sub(0 until 2, 0 until 1)

    require(sm1.numberRows == 2 && sm1.numberCols ==1)
    require(m2 == sm1)

  }

  def string2matrix{
    val a = new Matrix[Double](2,2) <= "1,0;0,1"
    val b = new Matrix[Double](2,2)
    b.eye
    require( a == b)
  }
  def testScalar(rowMajor:Boolean){
    import Scalar2MatrixConversions._
    val m1 = new Matrix[Double](3,3,rowMajor)
    m1.set(0,0,1.0)
    m1.set(0,1,2.0)
    m1.set(0,2,3.0)
    m1.set(1,1,4.0)
    m1.set(1,2,5.0)
    m1.set(2,2,6.0)


    m1.set(1,0,2.0)
    m1.set(2,0,3.0)
    m1.set(2,1,5.0)



    val rProda = 2.0 * m1
    val rProdb = m1*2.0
    require(rProda == rProdb)
    require(2.0+m1 == m1+2.0)
    require(2.0-m1 == m1-2.0)




    val m2 = new Matrix[Double](3,3,rowMajor)
    m2.set(0,0,1.0)
    m2.set(0,1,1.0/2.0)
    m2.set(0,2,1.0/3.0)
    m2.set(1,1,1.0/4.0)
    m2.set(1,2,1.0/5.0)
    m2.set(2,2,1.0/6.0)
    m2.set(1,0,1.0/2.0)
    m2.set(2,0,1.0/3.0)
    m2.set(2,1,1.0/5.0)



    val r2 = 1.0 / m1

    require(r2 == m2)


  }


  def testSerialization{
    import Scalar2MatrixConversions._
    val m1 = new Matrix[Double](3,3,true)
    m1.set(0,0,1.0)
    m1.set(0,1,2.0)
    m1.set(0,2,3.0)
    m1.set(1,1,4.0)
    m1.set(1,2,5.0)
    m1.set(2,2,6.0)


    m1.set(1,0,2.0)
    m1.set(2,0,3.0)
    m1.set(2,1,5.0)

    val output = new ObjectOutputStream(new FileOutputStream("C:\\temp\\test.obj"))
    output.writeObject(m1)
    output.close()

    val input = new ObjectInputStream(new FileInputStream("C:\\temp\\test.obj"))
    val obj = input.readObject()
    input.close()
    val m2 = obj.asInstanceOf[Matrix[Double]]

    require(m1 == m2)
  }

  def main(args:Array[String]){

    testToFromJama(rowMajor = true)
    testToFromJama(rowMajor =false)
    testProduct(rowMajor =true)
    testProduct(rowMajor =false)
    testInversion(rowMajor =true)
    testInversion(rowMajor =false)

    testTranspose(rowMajor =true)
    testTranspose(rowMajor =false)

    testCholesky(rowMajor = true)
    testCholesky(rowMajor = false)

    testEigen(rowMajor = true)
    testEigen(rowMajor = false)

    testScalar(rowMajor = true)
    testScalar(rowMajor = false)


    testSubMatrix(rowMajor = true)
    testSubMatrix(rowMajor = false)

    string2matrix

    testSerialization
  }



}
