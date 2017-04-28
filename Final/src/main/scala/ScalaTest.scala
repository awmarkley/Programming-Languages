package main.scala

import java.util.concurrent.TimeUnit
import java.io._

import main.java.GenericJava

import scala.reflect.ClassTag
import scala.util.Random._

/**
  * Created by Andrew Markley on 4/24/17.
  */
object ScalaTest {

  def main( args: Array[String] ): Unit = {
    val test = new GenericJava()

//    for ( size <- 500 to 3000 by 500 ) {
//      testDouble(size)
//    }

//    testInt(100)
    for ( size <- 500 to 3000 by 500 ) {
          testInt(size)
    }
//    testFloat(100)
//    testFloat(500)
//    testFloat(1000)
//    testFloat(1500)
//    testFloat(2000)
//    testFloat(2500)
//    testFloat(3000)

//    testLong(100)
//    testLong(500)
//    testLong(1000)
//    testLong(1500)
//    testLong(2000)
//    testLong(2500)
//    testLong(3000)
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

  def testLong(size: Int) : Unit = {
    val matrix = Array.fill(size, size) {
      nextLong + 1
    }
    val vector = Array.fill(size) {
      nextLong + 1
    }

    val elapsed = getResult(matrix, vector)

    printTest(size, vector(0).getClass, elapsed)
  }

  def getResult[T](matrix: Array[Array[T]], vector: Array[T])(implicit num: Numeric[T], classTag: ClassTag[T]): Long = {
    val start = System.nanoTime()
    val result = Generic.solve(matrix, vector)

    TimeUnit.MILLISECONDS.convert( System.nanoTime() - start, TimeUnit.NANOSECONDS )
  }
  def printTest( size: Int, clazz: Class[_], elapsed: Long ): Unit = {
    println( clazz + ": " + size + "x" + size + " takes " + elapsed + " milliseconds" )
  }

}
