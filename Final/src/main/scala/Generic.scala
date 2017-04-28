package main.scala

/**
  * Created by awmarkley on 4/18/17.
  */

import scala.reflect.ClassTag

object Generic {

  def main(args: Array[String] ): Unit = {

    val a = Array(
      Array(1.0, 2.0, 3.0),
      Array(2.0, -1.0, 1.0),
      Array(3.0, 0.0, -1.0))
    val b = Array(9.0, 8.0, 3.0)


    val x = solve(a, b)

    x.foreach{ println }

  }

  def solve[T]  ( matrix: Array[Array[T]], vector: Array[T] )
                (implicit num: Numeric[T],  tag: ClassTag[T]) : Array[T] = {

    import num._

    if ( matrix.length != matrix(0).length )
      throw new RuntimeException("You must supply a square matrix!")
    if ( matrix.length != vector.length )
      throw new RuntimeException("Columns of matrix must equal rows of vector.")

    val N = matrix.length

    for ( k <- 0 until N ) {

      //Where is the pivot row?
      var max = k
      for (i <- (k + 1) until N)
        if (compare(abs(matrix(i)(k)), abs(matrix(max)(k))) > 0)
          max = i

      //Swap pivot row into position
      val temp = matrix(k)
      matrix(k) = matrix(max)
      matrix(max) = temp

      //Update vector values to match matrix
      val t = vector(k)
      vector(k) = vector(max)
      vector(max) = t

      //Add multiples of one row to another to create an upper triangle
      for (i <- (k + 1) until N) {
        val factor = divide(matrix(i)(k), matrix(k)(k))
        vector(i) -= factor * vector(k)
        for (j <- k until N)
          matrix(i)(j) -= factor * matrix(k)(j)
      }
    }

    //If Determinant of upper triangle matrix is 0, the matrix is singular.
    if ( !invertible(matrix) )
      throw new RuntimeException("Matrix is singular, there are no solutions.")

    val solution = new Array[T](N)

    for ( i <- (N-1) to 0 by -1) {
      var sum: T = zero

      for ( j <- (i+1) until N )
        sum += matrix(i)(j) * solution(j)

      solution(i) = divide( vector(i) - sum, matrix(i)(i) )
    }

    solution
  }

  def typeCast[T,S](value: S )(implicit num: Numeric[S]): T = {
    import num._

    try {
      value.asInstanceOf[T]
    } catch {
      case _: Throwable => value.toDouble.asInstanceOf[T]
    }
  }

  def divide[T]( x: T, y: T )
               (implicit num: Numeric[T]): T = {

    import num._

    try {
      x * Math.pow(y.toDouble, -1).asInstanceOf[T]
    } catch {
      case _ : ClassCastException =>
        (x.toDouble() * Math.pow(y.toDouble, -1)).toInt.asInstanceOf[T]
    }
  }

  def invertible[T]( matrix: Array[Array[T]] )
                          (implicit num: Numeric[T]) : Boolean = {

    for ( i <- matrix.indices ) { if ( matrix(i)(i) == 0) return false; }

    true
  }
}
