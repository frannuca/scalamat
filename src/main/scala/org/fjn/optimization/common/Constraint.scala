package org.fjn.optimization.common

import org.fjn.matrix.Matrix

sealed trait ConstaintType

object CONSTRAINT_TYPES{

  case object EQ extends ConstaintType
  case object LT extends ConstaintType
  case object LE extends ConstaintType
  case object GE extends ConstaintType
}

case class Constraint(A:Matrix[Double],b:Matrix[Double],cType:ConstaintType)