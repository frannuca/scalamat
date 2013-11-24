package org.fjn.optimization.contraint

import org.fjn.matrix.Matrix



private trait Lagrange{
type DMatrix = Matrix[Double]
  val pFunc:(DMatrix)=>Double
  val A:DMatrix
  val b:DMatrix
  val lambda:DMatrix

  val laplaceFunction= (x:DMatrix) =>{
    pFunc(x)+ (lambda.transpose * (A*x-b))(0,0)
  }

}

