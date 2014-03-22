package org.fjn.matrix

import scala.Array
import java.util.{HashMap, ArrayList}
import scala.collection.JavaConversions._

import scala.util.Random
import com.sun.org.apache.bcel.internal.generic.ClassObserver
import org.fjn.matrix


object MatrixExtensions{

  implicit def toMatrix(s:Seq[Double])=new{
    def toMatrix={
      val m = new Matrix[Double](s.length,1)
      m <= s
      m
    }

  }
}

class Matrix[T1](nRows: Int, nCols: Int, isRowMajor: Boolean = false)(implicit m2: Manifest[T1], implicit val m: Fractional[T1])
extends Serializable{
  outer =>

  val isConfiguredAsRowMajor = isRowMajor

  type DataType = T1

  def <=(x: Seq[T1]): Matrix[T1] = {
    require(x.length == this.numberRows && this.numberCols == 1)
    x.toArray.copyToArray(this.data, 0)
    this
  }

  def <=(x: String): Matrix[T1] = {
    val rows: Array[String] = x.split(";")
    require(rows.length == this.numberRows)

    for (i <- 0 until rows.length) {
      val cols = rows(i).split(",")
      for (j <- 0 until cols.length) {
        set(i, j, cols(j).toDouble.asInstanceOf[T1])
      }
    }
    this
  }

  def <=(f: (T1) => T1): Matrix[T1] = {
    this.data = this.data.map(f)
    this
  }

  def random:Matrix[T1]= {
    val rnd = new Random()
    this.data = this.data.map(x => rnd.nextDouble().asInstanceOf[T1])
    this
  }

  def randomEye:Matrix[T1]= {
    val rnd = new Random()
    this.data = this.data.map(x => rnd.nextDouble().asInstanceOf[T1])

      for(i<- 0 until this.numberCols;
          j <- 0 until this.numberRows){
        if(i!=j)
          this.set(i,j,0.toDouble.asInstanceOf[T1])
      }

    this
  }

  def getArray(): Array[T1] = data

  private var data: Array[T1] = new Array[T1](nCols * nRows)

  def apply(row: Int, col: Int): T1 = {

    val index = isRowMajor match {
      case true => col + row * numberCols
      case false => row + col * numberRows
    }

    data(index)
  }

  def getColArray(i: Int): Array[T1] = {

    val start = if (isRowMajor) i else i * this.numberRows
    val step = if (isRowMajor) this.numberCols else 1
    var counter = 0
    val rArr = new Array[T1](this.numberRows)
    while (counter < this.numberRows) {
      rArr(counter) = data(start + step * counter)
      counter = counter + 1
    }
    rArr
  }


  def getColArrayIndexer(i: Int): (Int, Int) = {
    (if (isRowMajor) i else i * this.numberRows, if (isRowMajor) this.numberCols else 1)
  }


  trait MatrixLineIterator extends Iterator[T1] {
    protected var position: Int = -1
    protected val start: Int
    protected val step: Int
    val maxCount: Int

    def hasNext: Boolean = {
      (position + 1) < maxCount
    }


    def next: T1 = {
      try {
        position = position + 1
        outer.data(start + (position) * step)
      } catch {
        case e: Exception => {
          println(e.toString)
          m.zero
        };


      }


    }

    def reset(): Unit = {
      position = -1
      Unit
    }

  }

  class RowsIterator(row: Int) extends MatrixLineIterator {
    val (a, b) = getRowArrayIndexer(row)
    val start = a;
    val step = b
    val maxCount = outer.numberCols
  }

  class ColsIterator(col: Int) extends MatrixLineIterator {
    val (a, b) = getColArrayIndexer(col)
    val start = a;
    val step = b
    val maxCount = outer.numberCols
  }

  def getColArrayIterator(i: Int): MatrixLineIterator = {
    new ColsIterator(i)
  }


  def getRowArray(j: Int): Array[T1] = {

    val start = if (isRowMajor) j * this.numberCols else j
    val step = if (isRowMajor) 1 else this.numberRows
    var counter = 0
    val rArr = new Array[T1](this.numberCols)
    while (counter < this.numberCols) {
      rArr(counter) = data(start + step * counter)
      counter = counter + 1
    }
    rArr
  }

  def getRowArrayIndexer(j: Int): (Int, Int) = {
    (if (isRowMajor) j * this.numberCols else j, if (isRowMajor) 1 else this.numberRows)
  }

  def getRowArrayIterator(i: Int): MatrixLineIterator = {
    new RowsIterator(i)
  }

  private var numberRows_ = nRows
  private var numberCols_ = nCols

  def numberRows = numberRows_

  def numberCols = numberCols_

  def zeros: Matrix[T1] = {
    var i: Int = 0;
    while (i < numberRows) {
      var j: Int = 0
      while (j < numberCols) {
        this.set(i, j, m.zero)
        j += 1
      }
      i = i + 1
    }
    this
  }


  def eye = {
    zeros
    val limit = math.min(numberRows, numberCols)
    var i = 0
    while (i < limit) {
      this.set(i, i, m.one)
      i = i + 1

    }
  }


  def update[T2 <% T1](row: Int, col: Int, v: T2): Unit = {
    set(row,col,v)
  }
  def set[T2 <% T1](row: Int, col: Int, v: T2): Unit = {

    val index = isRowMajor match {
      case true => col + row * numberCols
      case false => row + col * numberRows
    }

    data(index) = v

    Unit
  }


  def /[T2<%T1](a: T2): Matrix[T1] = {

    val rMatrix: Matrix[T1] = new Matrix[T1](this.numberRows, this.numberCols, isConfiguredAsRowMajor);
    var i = 0
    while (i < rMatrix.data.length) {
      rMatrix.data(i) = m.div(this.data(i), a)
      i = i + 1
    }

    rMatrix
  }


  def *[T2<%T1](a: T2): Matrix[T1] = {

    val rMatrix: Matrix[T1] = new Matrix[T1](this.numberRows, this.numberCols, isConfiguredAsRowMajor);
    var i = 0
    while (i < rMatrix.data.length) {
      rMatrix.data(i) = m.times(this.data(i), a)
      i = i + 1
    }

    rMatrix
  }

  def +(b: T1): Matrix[T1] = {
    val resM = this.clone()
    var i = 0;
    while (i < data.length) {
      resM.data(i) = m.plus(data(i), b)
      i = i + 1
    }
    resM
  }

  def +(b: Matrix[T1]): Matrix[T1] = {
    require(this.numberCols == b.numberCols && this.numberRows == b.numberRows)
    val rMatrix: Matrix[T1] = new Matrix[T1](this.numberRows, this.numberCols, isConfiguredAsRowMajor);
    var i = 0
    var j = 0
    while (i < rMatrix.numberRows) {
      j = 0

      val iterator2 = b.getRowArrayIterator(i)
      val iterator1 = this.getRowArrayIterator(i)
      while (j < rMatrix.numberCols) {
        rMatrix.set(i, j, m.plus(iterator1.next, iterator2.next))
        j = j + 1
      }
      i = i + 1
    }

    rMatrix

  }

  def -(b: Matrix[T1]): Matrix[T1] = {
    require(this.numberCols == b.numberCols && this.numberRows == b.numberRows)
    val rMatrix: Matrix[T1] = new Matrix[T1](this.numberRows, this.numberCols, isConfiguredAsRowMajor);
    var i = 0
    var j = 0
    while (i < rMatrix.numberRows) {
      j = 0

      val iterator2 = b.getRowArrayIterator(i)
      val iterator1 = this.getRowArrayIterator(i)
      while (j < rMatrix.numberCols) {
        rMatrix.set(i, j, m.minus(iterator1.next, iterator2.next))
        j = j + 1
      }
      i = i + 1
    }

    rMatrix

  }

  def -(b: T1): Matrix[T1] = {
    val resM = this.clone()
    var i = 0;
    while (i < data.length) {
      resM.data(i) = m.minus(data(i), b)
      i = i + 1
    }
    resM
  }

  override def clone(): Matrix[T1] = {
    val out = new Matrix[T1](this.numberRows, this.numberCols, this.isConfiguredAsRowMajor)

    outer.data.copyToArray(out.data, 0)
    out
  }

  def copyFrom(that: Matrix[T1]) {
    if (that.numberCols == numberCols && that.numberRows == numberRows) {
      for (j <- 0 until numberCols;
           i <- 0 until numberRows) {
        set(i, j, that(i, j))
      }
    }
    else {
      throw new Exception("inompatible size applied to matrix copyFrom operation")
    }
  }

  def unary_+ : Matrix[T1] = {
    val out = new Matrix[T1](this.numberRows, this.numberCols)
    this.data.copyToArray(out.data, 0)
    out
  }

  def unary_- : Matrix[T1] = {
    val out = new Matrix[T1](this.numberRows, this.numberCols)
    out.data = this.data.map(x => m.minus(m.zero, x))
    out
  }

  def *(b: Matrix[T1]): Matrix[T1] = {

    case class RowColMsg(rowIndex: Int, colIndex: Int, a1: Array[T1], a2: Array[T1])

    require(this.numberCols == b.numberRows)
    val rMatrix: Matrix[T1] = new Matrix[T1](this.numberRows, b.numberCols, isConfiguredAsRowMajor);
    var i = 0
    var j = 0


    var k = 0
    while (i < rMatrix.numberRows) {

      j = 0
      val iterator1 = outer.getRowArrayIterator(i)
      while (j < b.numberCols) {
        val iterator2 = b.getColArrayIterator(j)
        var r: T1 = m.zero

        k = 0
        while (k < b.numberRows) {
          r = m.plus(r, m.times(iterator1.next, iterator2.next))
          k = k + 1
        }

        rMatrix.set(i, j, r)
        j = j + 1
        iterator1.reset
      }
      i = i + 1
    }
    rMatrix
  }

  override def toString = {
    val rStr = new StringBuilder
    var i = 0;
    var j = 0;
    while (i < numberRows) {
      j = 0
      while (j < numberCols) {
        rStr.append(this.apply(i, j))
        rStr.append(",")
        j = j + 1;
      }
      rStr.append(';')
      i = i + 1
    }

    rStr.toString()

  }

  /**
   * TODO: Does a hard copy of the matrix, not good for large systems
   */
  def invert {
    //MatrixInterface.invert(data.asInstanceOf[Array[Double]],this.numberCols)

    import Jama.{_}

    val A = toJama
    val I = Jama.Matrix.identity(this.numberRows, this.numberCols);
    val s = A.solve(I);
    fromJama(s)
  }

  def svd(): (Jama.Matrix, Jama.Matrix, Jama.Matrix) ={
    import Jama._
    val A = toJama
    val sv = new  Jama.SingularValueDecomposition(A)
    (sv.getU,sv.getS,sv.getV)
  }

  def fromJama(m: Jama.Matrix) {

    if (isRowMajor)
      this.data = m.getRowPackedCopy.toList.map(t => t.asInstanceOf[T1]).toArray
    else
      this.data = m.getColumnPackedCopy.toList.map(t => t.asInstanceOf[T1]).toArray


  }

  def toJama: Jama.Matrix = {


    val m = new Jama.Matrix(this.numberRows, this.numberCols)
    for (i <- 0 until this.numberRows;
         j <- 0 until this.numberCols) {
      m.set(i, j, this.apply(i, j).asInstanceOf[Double])
    }
    m


  }

  def transpose: Matrix[T1] = {


    val A = clone.toJama

    val trans = new Matrix[T1](numberCols, numberRows)

    trans.fromJama(A.transpose())

    trans
  }


  override def hashCode() = super.hashCode()

  override def equals(obj: Any) = {

      Option(obj.asInstanceOf[Matrix[T1]]) match {
        case Some(that) => {
          val isSame = (that.numberCols == this.numberCols) &&
            (that.numberRows == this.numberRows) &&
            that.isConfiguredAsRowMajor == this.isConfiguredAsRowMajor &&
            (that.data zip this.data).forall(x => math.abs(m.minus(x._1, x._2).asInstanceOf[Double]) < 1e-12)

          isSame
        }
        case None => false
      }



  }


  def det: T1 = {
    val A = toJama
    A.det().asInstanceOf[T1]
  }

  def eigVectors: (Matrix[T1], Matrix[T1]) = {

    val A = toJama
    val ev = A.eig()

    val evD = ev.getD
    val eigenMatrixD = new Matrix[T1](evD.getRowDimension, evD.getColumnDimension, isConfiguredAsRowMajor)
    eigenMatrixD.fromJama(evD)

    val evV = ev.getV
    val eigenMatrixV = new Matrix[T1](evV.getRowDimension, evV.getColumnDimension, isConfiguredAsRowMajor)
    eigenMatrixV.fromJama(evV)


    (eigenMatrixD, eigenMatrixV)
  }

  def cholesky: Matrix[T1] = {

    val A = toJama
    val s = A.chol().getL
    val result: Matrix[T1] = new Matrix[T1](s.getRowDimension, s.getColumnDimension, isConfiguredAsRowMajor)
    result.fromJama(s)
    result
  }

  def sub(rows: Seq[Int], cols: Seq[Int]): Matrix[T1] = {
    val r = new Matrix[T1](rows.length, cols.length, isRowMajor)
    for (ir <- rows.indices;
         cr <- cols.indices) {
      r.set(ir, cr, this.apply(rows(ir),cols(cr)))
    }

    r
  }


  /**
   * apply a matrix mask to this matrix. A mask operation consist on multiplying each position in this matrix with
   * its corresponding position in the mask. The size of the mask must be smaller or equal than the size of this matrix.
   * If the number of columns or rows in the mask is smaller than rank of this matrix then the mask is only applied
   * to this portion of the matrix
   * @param mask
   */
  def <:=(mask: Matrix[T1]) {
    if (mask.numberCols <= numberCols && mask.numberRows <= numberRows) {
      for (i <- 0 until mask.numberRows;
           j <- 0 until mask.numberCols) {
        set(i, j, m.times(this(i, j), mask(i, j)))
      }
    }

  }


  def diag:Seq[T1]={

    (for(i <- 0 until (this.numberCols min this.numberRows)) yield {
      this(i,i)
    }).toSeq
  }


  def setSubMatrix(sub:Matrix[T1],i:Int,j:Int){
    require(j + sub.numberCols < numberCols)
    require(i + sub.numberRows < numberRows)

    for(n <- 0 until sub.numberRows;
        m <- 0 until sub.numberCols){
      this(i+n,j+m)=sub(n,m)
    }

  }

}


