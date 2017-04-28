package main.java;

import java.lang.reflect.Array;
import java.text.DecimalFormat;
import java.util.Arrays;

/**
 * Created by Andrew Markley on 4/12/17.
 */
@SuppressWarnings("unchecked")
public class GenericJava {


    public static void main( String[] args ) {

        Double[][] a = {
                { 1.0 , 2.0, 3.0},
                { 2.0, -1.0, 1.0},
                { 3.0, 0.0, -1.0}};
        Double[] b =
                { 9.0 , 8.0, 3.0 };
        GenericJava tester = new GenericJava();

        Number[] x = tester.solve( a, b );

        tester.print( a, b, x);
        System.out.println( "\n" + Arrays.toString(x) );
    }

    public GenericJava() { super(); }

    private <E extends Number> void print( E[][] matrix, E[] vector, E[] solution ) {
        StringBuilder s = new StringBuilder();
        DecimalFormat two = new DecimalFormat("0.00");

        for ( int row = 0; row < matrix.length; row++ ) {
            s.append("|");
            for ( int col = 0; col < matrix.length; col++ ) {
                s.append( " " + two.format(matrix[row][col]) + " " );
            }
            s.append("|   | " + two.format(solution[row] )
                    + " | = | " + two.format(vector[row]) + " | ");
            s.append("|\n");
        }

        System.out.print( s.toString() );
    }

    public <E extends Number> E[] solve( E[][] matrix, E[] vector) {

        if ( matrix.length != matrix[0].length )
            throw new RuntimeException("You must supply a square matrix!");
        if ( matrix.length != vector.length )
            throw new RuntimeException("Columns of matrix must equal rows of vector.");

        int N = matrix.length;
        for (int k = 0; k < N; k++) {

            //Where is the pivot row?
            int max = k;
            for (int i = k + 1; i < N; i++)
                if ( compare( abs(matrix[i][k]), abs(matrix[max][k])) > 0 )
                    max = i;

            //Swap pivot row into position
            E[] temp = matrix[k];
            matrix[k] = matrix[max];
            matrix[max] = temp;

            //Update vector values to match matrix
            E t = vector[k];
            vector[k] = vector[max];
            vector[max] = t;

            //Add multiples of one row to another to reach
            for (int i = k + 1; i < N; i++) {
                E factor = divide( matrix[i][k] , matrix[k][k]);
                vector[i] = minus( vector[i], times(factor, vector[k] ));
                for (int j = k; j < N; j++)
                    matrix[i][j] = minus( matrix[i][j] ,
                                   times( factor, matrix[k][j]));
            }
        }

        if ( !invertible(matrix) )
            throw new RuntimeException( "Matrix is singular, there are no solutions.");

        E[] solution = (E[]) Array.newInstance( Number.class, vector.length );

        for (int i = N - 1; i >= 0; i--) {
            E sum = generic( 0 , matrix[0][0].getClass());
            for (int j = i + 1; j < N; j++)
                sum = plus( sum, times( matrix[i][j], solution[j] ));

            solution[i] = (E)
                    divide( minus( vector[i], (E) sum), matrix[i][i] );
        }

        return solution;
    }

    private <E extends Number> boolean invertible( E[][] matrix ) {
        for ( int i = 0; i < matrix.length; i++ ) {
            if ( compare( matrix[i][i], 0) == 0 ) return false;
        }

        return true;
    }

    private <E extends Number> E times ( E x, E y ) {

        //Floating point
        if ( x instanceof Double && y instanceof Double )
            return (E) Double.valueOf((Double) x * (Double) y) ;
        else if ( x instanceof Float && y instanceof Float)
            return (E) Float.valueOf((Float) x * (Float) y);
        //Integral
        else if ( x instanceof Long && y instanceof Long )
            return (E) Long.valueOf((Long) x * (Long) y);
        else if ( x instanceof Integer && y instanceof Integer )
            return (E) Integer.valueOf((Integer) x * (Integer) y);
        else
            throw new RuntimeException("Unsupported type " + x.getClass());
    }

    private <E extends Number> E divide ( E x, E y ) {
        //Floating point
        if ( x instanceof Double )
            return (E) Double.valueOf((Double) x / (Double) y);
        else if ( x instanceof Float && y instanceof Float )
            return (E) Float.valueOf((Float) x / (Float) y);
            //Integral
        else if ( x instanceof Long && y instanceof Long )
            return (E) Long.valueOf((Long) x / (Long) y);
        else if ( x instanceof Integer && y instanceof Integer )
            return (E) Integer.valueOf((Integer) x / (Integer) y);
        else
            throw new RuntimeException("Unsupported type: " + x.getClass());
    }

    private <E extends Number> E plus ( E x, E y ) {
        //Floating point
        if ( x instanceof Double && y instanceof Double )
            return (E) Double.valueOf((Double) x + (Double) y);
        else if ( x instanceof Float && y instanceof Float )
            return (E) Float.valueOf((Float) x + (Float) y );
        //Integral
        else if ( x instanceof Long && y instanceof Long )
            return (E) Long.valueOf((Long) x + (Long) y);
        else if ( x instanceof Integer && y instanceof Integer )
            return (E) Integer.valueOf((Integer) x + (Integer) y);
        else
            throw new RuntimeException("Unsupported type: " + x.getClass());

    }

    private <E extends Number> E minus ( E x, E y ) {
        //Floating point
        if ( x instanceof Double && y instanceof Double )
            return (E) Double.valueOf((Double) x - (Double) y);
        else if ( x instanceof Float && y instanceof Float )
            return (E) Float.valueOf((Float) x - (Float) y );
        //Integral
        else if ( x instanceof Long && y instanceof Long )
            return (E) Long.valueOf((Long) x - (Long) y);
        else if ( x instanceof Integer && y instanceof Integer )
            return (E) Integer.valueOf((Integer) x - (Integer) y);
        else
            throw new RuntimeException("Unsupported type: " + x.getClass());

    }

    private <E extends Number> E abs( E val ) {
        if ( compare( val, (E) Double.valueOf(0)) >= 0 )
            return val;
        else
            return (E) times( generic( 1, val.getClass()), val );
    }

    private <E extends Number> E generic( Number val, Class<?> clazz ) {
        if ( clazz.equals(Double.class))
            return (E) Double.valueOf(val.doubleValue());
        else if ( clazz.equals(Integer.class))
            return (E) Integer.valueOf(val.intValue());
        else if ( clazz.equals(Float.class))
            return (E) Float.valueOf(val.floatValue());
        else if ( clazz.equals(Long.class))
            return (E) Long.valueOf(val.longValue());
        else
            throw new RuntimeException("Unsupported type: " + clazz );
    }

    private <E extends Number> int compare(E x, E y) {

        if ( x.doubleValue() > y.doubleValue() )
            return 1;
        else if ( x.doubleValue() < y.doubleValue() )
            return -1;
        else
            return 0;
    }
}
