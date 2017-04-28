package main.java;

import java.util.Random;
import java.util.concurrent.TimeUnit;

/**
 * Created by Andrew Markley on 4/24/17.
 */
public class JavaTest {

    public static void main( String[] args ) {

//        for ( int size = 500; size <= 3000; size += 500 ) {
//            testInt(size);
//        }
//
//        for ( int size = 500; size <= 3000; size += 500 ) {
//            testDouble(size);
//        }

        for ( int size = 500; size <= 3000; size += 500 ) {
            testFloat(size);
        }


    }

    private static void testDouble(int size) {
        Double[][] matrix = new Double[size][size];
        Double[] vector = new Double[size];

        Random rand = new Random();

        for ( int i = 0; i < matrix.length; i++ ) {
            vector[i] = ( rand.nextDouble() - 0.5 ) * 10;
            for ( int j = 0; j < matrix.length; j++ ) {
                matrix[i][j] = ( rand.nextDouble() - 0.5 ) * 10;
            }
        }

        long elapsed = getResult(matrix, vector);
        printTest(size, Double.class, elapsed);
    }

    private static void testFloat(int size) {
        Float[][] matrix = new Float[size][size];
        Float[] vector = new Float[size];

        Random rand = new Random();

        for ( int i = 0; i < matrix.length; i++ ) {
            vector[i] = rand.nextFloat() * 10 - 20;
            for ( int j = 0; j < matrix.length; j++ ) {
                matrix[i][j] = rand.nextFloat() * 10 - 20;
            }
        }

        long elapsed = getResult(matrix, vector);
        printTest(size, Float.class, elapsed);
    }

    private static void testInt(int size) {
        Integer[][] matrix = new Integer[size][size];
        Integer[] vector = new Integer[size];

        Random rand = new Random();

        for ( int i = 0; i < matrix.length; i++ ) {
            vector[i] = rand.nextInt(10) - 20;
            for ( int j = 0; j < matrix.length; j++ ) {
                matrix[i][j] = rand.nextInt(10) - 20;
            }
        }

        long elapsed = getResult(matrix, vector);
        printTest(size, Integer.class, elapsed);
    }

    private static<E extends Number> long getResult(E[][] matrix, E[] vector) {
        long start = System.nanoTime();
        GenericJava tester = new GenericJava();
        E[] result = tester.solve(matrix, vector);

        return TimeUnit.MILLISECONDS.convert( System.nanoTime() - start,
                TimeUnit.NANOSECONDS);
    }

    private static<E> void printTest( int size, Class<E> clazz, long elapsed ) {
        System.out.println(clazz + ": " + size + "x" + size
                + " takes " + elapsed + " milliseconds" );
    }
}
