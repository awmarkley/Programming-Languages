package main.scala

import java.util.concurrent.TimeUnit
import scala.reflect.ClassTag
import scala.util.Random._

/**
  * Created by Andrew Markley on 4/24/17.
  */
object ScalaTest {

  def main( args: Array[String] ): Unit = {

    for ( size <- 500 to 3000 by 500 )  testInt(size)

//    for ( size <- 500 to 3000 by 500 )  testDouble(size)


//    for ( size <- 500 to 3000 by 500 )  testFloat(size)
  }

  def testDouble(size: Int ) : Unit = {
    val matrix = Array.fill(size, size) {
      (nextDouble - 0.5) * 10
    }
    val vector = Array.fill(size) {
      (nextDouble - 0.5) * 10
    }

    val elapsed = getResult(matrix, vector)

    printTest(size, vector(0).getClass, elapsed)
  }

  def testInt(size: Int) : Unit = {
    val matrix = Array.fill(size, size) {
      nextInt(10) + 1
    }
    val vector = Array.fill(size) {
      nextInt(10) + 1
    }

    val elapsed = getResult(matrix, vector)

    printTest(size, vector(0).getClass, elapsed)
  }

  def testFloat(size: Int) : Unit = {
    val matrix = Array.fill(size, size) {
      (nextFloat - 0.5) * 10
    }
    val vector = Array.fill(size) {
      (nextFloat - 0.5) * 10
    }

    val elapsed = getResult(matrix, vector)

    printTest(size, vector(0).getClass, elapsed)
  }

  def getResult[T](matrix: Array[Array[T]], vector: Array[T])
                  (implicit num: Numeric[T], classTag: ClassTag[T]): Long = {
    val start = System.nanoTime()
    val result = Generic.solve(matrix, vector)

    TimeUnit.MILLISECONDS.convert( System.nanoTime() - start, TimeUnit.NANOSECONDS )
  }
  def printTest( size: Int, clazz: Class[_], elapsed: Long ): Unit = {
    println( clazz + ": " + size + "x" + size + " takes " + elapsed + " milliseconds" )
  }

}
