/***************************************************
 * intro111/ipis111: Third assignment              *
 *                                                 *
 * This class is for assignment #3 - Part 2        *
 *                                                 *
 * Author(s): ### YOUR NAME(S) AND EMAIL(S). ##### *
 * Date: ##/##/####                                *
 *                                                 *
 ***************************************************/

/*
 * Important! Add comments to every method!
 *
 * The "return" statement at the end of each method and the initial value 
 * is just so this skeleton file compiles, change it according to your needs
 */

public class Ex2 {

    public static double distance(int[] point1, int[] point2) {
        return Math.sqrt(Math.pow(point1[0] - point2[0], 2)
                + Math.pow(point1[1] - point2[1], 2));
    }

    public static boolean lexGreaterThan(int[] p1, int[] p2) {
        return (p1[0] > p2[0]) || (p1[0] == p2[0] && p1[1] > p2[1]);
    }

    /******************** Task 1 ********************/
    public static int[][] findClosestSimple(int[][] points) {
        double distance,    // Temp variable.
               min_d = -1;  // Minimal distance.

        int[][] min_couple = new int[2][2];  // Pair of coords with minimal distance.

        // Error handling.
        if (points.length < 2) {
            return null;
        }

        // Choose the shortest distance between all couples:
        //     Iterate over each pair,
        //     and test its distance with the pairs ahead -
        //     (the ones that hadn't been tested against it with it yet).
        for (int i=0; i<points.length; i++) {
            for (int j=i+1; j<points.length; j++) {
                distance = distance(points[i], points[j]);

                // Initialize the minimum pair with the first two coords.
                if (min_d == -1) {
                    min_d = distance;

                    min_couple[0] = points[i];
                    min_couple[1] = points[j];
                // If the current distance is shorter than the ones found before,
                // choose these two pairs as the minimal couple.
                } else if (distance < min_d) {
                    min_d = distance;

                    min_couple[0] = points[i];
                    min_couple[1] = points[j];
                }
            }
        }

        return min_couple;
    }

    /******************** Task 2 ********************/
    public static int[][] split(boolean first_half, int[][] points) {
        int l      = points.length,
            half_l = points.length /2;

        int[][] ans;

        int i;

        // Return 1st/2nd half of the original points array.
        if (first_half) {
            ans = new int[half_l][2];

            for (i=0; i<half_l; i++) {
                ans[i][0] = points[i][0];
                ans[i][1] = points[i][1];
            }
        } else {  // 2nd half.
            ans = new int[l - half_l][2];

            for (i=half_l; i<l; i++) {
                ans[i - half_l][0] = points[i][0];
                ans[i - half_l][1] = points[i][1];
            }
        }

        return ans;
    }

    public static int[][] sort(boolean byX, int [][] points) {
        // Error handling.
        if (points == null || points.length < 2) {
            return points;
        }

        // "
        for (int i=0; i<points.length; i++) {
            if (points[i].length != 2) {
                return points;
            }
        }

        // Merge sort.
        return merge(byX,
                sort(byX, split(true,  points)),  // Left
                sort(byX, split(false, points))   // Right
               );
    }

    public static int[][] merge(boolean byX, int[][] half1, int[][] half2) {
        int len1 = half1.length,
            len2 = half2.length;

        int i  = 0,  // Result index.
            i1 = 0,  // 1st half index.
            i2 = 0;  // 2nd half index.

        int[][] ans = new int[len1 + len2][2];

        // Merge the two arrays while ordering them by point size:
        while(i1 < len1 && i2 < len2) {
            if (byX) {
                // For {a,b} and {c,d} - order by [a>c then b>d] .
                if ( ! lexGreaterThan(half1[i1], half2[i2]) ) {  // {a,b} < {c,d} .
                    ans[i][0] = half1[i1][0];
                    ans[i][1] = half1[i1][1];
                    i1++;
                } else {  // {a,b} > {c,d}  [Opposite case] .
                    ans[i][0] = half2[i2][0];
                    ans[i][1] = half2[i2][1];
                    i2++;
                }
            } else {  // by Y.
                // For {a,b} and {c,d} - order by [b>d then a>c] .
                if ( ! lexGreaterThanByY(half1[i1], half2[i2]) ) {  // {a,b} < {c,d} .
                    ans[i][0] = half1[i1][0];
                    ans[i][1] = half1[i1][1];
                    i1++;
                } else {  // {a,b} > {c,d}  [Opposite case] .
                    ans[i][0] = half2[i2][0];
                    ans[i][1] = half2[i2][1];
                    i2++;
                }
            }

            i++;
        }

        // Once one array (one half that is) was fully copied,
        // we need to copy the rest of the second half afterwards.
        for(; i1<len1 && i<ans.length; i1++, i++) {
            ans[i][0] = half1[i1][0];
            ans[i][1] = half1[i1][1];
        }
        for(; i2<len2 && i<ans.length; i2++, i++) {
            ans[i][0] = half2[i2][0];
            ans[i][1] = half2[i2][1];
        }

        return ans;
    }

    public static int[][] sortByLex(int[][] points) {
        return sort(true, points);  // Sort by [X then Y] coord
    }

    public static boolean lexGreaterThanByY(int[] p1, int[] p2) {
        // Same as lexGreaterThan, but sorts by [Y then X] coord instead of [X then Y].
        return (p1[1] > p2[1]) || (p1[1] == p2[1] && p1[0] > p2[0]);
    }

    public static int[][] sortByY(int[][] points) {
        return sort(false, points);
    }

    public static int[] duplicates(int[][] points) {
        // Error handling:
        // If we recieved only one coordinate, there's nothing to compare to.
        if (points == null || points.length < 2) {
            return null;
        }

        // Compare each coordinate to the rest ahead in the list.
        int l = points.length;
        for (int i=0; i<l; i++) {
            for (int j=i+1; j<l; j++) {
                if (points[i][0] == points[j][0] && points[i][1] == points[j][1]) {
                    return points[i];
                }
            }
        }

        // If there're no duplicates...
        return null;
    }

    public static int[][] filterPointsByRange(double fromX, double toX, int[][] points) {
        int size = 0;  // Determine size of filtered points array.
        int x;         // Holds X coordinate of the currently inspected point.

        for (int i=0; i<points.length; i++) {
            x = points[i][0];
            if (x >= fromX && x <= toX) {
                size++;
            }
        }

        int[][] filtered = new int[size][2];
        int j=0;  // Filtered array's index.

        // Re-run over the points and filter by X range.
        if (size > 0) {
            for (int i=0; i<points.length; i++) {
                x = points[i][0];
                if (x >= fromX && x <= toX) {
                    filtered[j][0] = x;
                    filtered[j][1] = points[i][1];
                    j++;
                }
            }
        }

        // NOTE: If we were allowed to use lists, this would've been more efficient.
        return filtered;
    }

    /******************** Task 3 ********************/
    public static int[][] findClosest(int[][] points) {
        int[][] res;
        int[][] pointsSortedByLex = sortByLex(points);
        int[] p = duplicates(pointsSortedByLex);
        if (p == null)
            res = findClosestRecursive(pointsSortedByLex);
        else {
            res = new int[1][];
            res[0] = p;
        }
        return res;
    }

    public static int[][] findClosestRecursive(int[][] points) {
        int[][] res = null;
        // YOUR CODE HERE
        return res;
    }

    /******************** Auxiliary functions ********************/

    /**
     * @param arr
     *          the input 2D array
     * @return a string representation of a 2D array
     */
    public static String matrixToString(int[][] arr) {
        String ret = "{ ";

        if (arr == null)
            ret = "null";
        else {
            for (int i = 0; i < arr.length; i++) {
                if (arr[i] != null) {
                    ret += "{ ";
                    for (int j = 0; j < arr[i].length; j++)
                        ret += arr[i][j] + " ";
                    ret += "} ";
                }
            }
            ret += "}";
        }

        return ret;
    }

    /**
     * @param arr the input array
     * @return a string representation of an array
     */
    public static String arrayToString(int[] arr) {
        String ret = "{ ";

        if (arr == null)
            ret = "null";
        else {
            for (int i = 0; i < arr.length; i++)
                ret += arr[i] + " ";
            ret += "}";
        }

        return ret;
    }

    public static void main(String[] args) {
        // Test task 1
        int[][] test1in = { { 9, 5 }, { 2, 9 }, { 2, 6 }, { 8, 6 }, { 1, 2 },
                { 1, 3 }, { 8, 9 }, { 0, 7 }, { 5, 9 }, { 9, 8 } };
        int[][] test1exp = { { 1, 2 }, { 1, 3 } };
        System.out.println("Test 1: expected=" + matrixToString(test1exp)
                + " actual=" + matrixToString(findClosestSimple(test1in)));

        // Test task 2a
        int[][] test2in = { { 1, 2 }, { 0, 7 }, { 2, 9 }, { 2, 6 }, { 1, 3 } };
        int[][] test2exp = { { 0, 7 }, { 1, 2 }, { 1, 3 }, { 2, 6 }, { 2, 9 } };
        System.out.println("Test 2: expected=" + matrixToString(test2exp)
                + " actual=" + matrixToString(sortByLex(test2in)));

        // Test task 2b
        int[][] test3in = { { 9, 8 }, { 5, 9 }, { 9, 5 }, { 8, 9 }, { 8, 6 } };
        int[][] test3exp1 = { { 9, 5 }, { 8, 6 }, { 9, 8 }, { 8, 9 }, { 5, 9 } };
        int[][] test3exp2 = { { 9, 5 }, { 8, 6 }, { 9, 8 }, { 5, 9 }, { 8, 9 } };
        System.out.println("Test 3: expected=" + matrixToString(test3exp1) + " or "
                + matrixToString(test3exp2) + " actual="
                + matrixToString(sortByY(test3in)));

        // Test task 2c
        int[][] test4in = { { 0, 7 }, { 1, 2 }, { 1, 3 }, { 2, 6 }, { 2, 9 } };
        int[] test4exp = null;
        System.out.println("Test 4: expected=" + arrayToString(test4exp)
                + " actual=" + arrayToString(duplicates(test4in)));

        int[][] test5in = { { 0, 7 }, { 1, 2 }, { 1, 2 }, { 2, 6 }, { 2, 6 } };
        int[] test5exp1 = { 1, 2 };
        int[] test5exp2 = { 2, 6 };
        System.out.println("Test 5: expected=" + arrayToString(test5exp1) + " or "
                + arrayToString(test5exp2) + " actual="
                + arrayToString(duplicates(test5in)));

        // Test task 2d
        int[][] test6in = { { 3, 0 }, { 3, 5 }, { 4, 3 }, { 6, 4 }, { 7, 3 } };
        int[][] test6exp = { { 3, 0 }, { 3, 5 }, { 4, 3 } };
        System.out.println("Test 6: expected=" + matrixToString(test6exp)
                + " actual=" + matrixToString(filterPointsByRange(2.9, 4.2, test6in)));

        // Test task 3
         int[][] test7in = {{0,7},{1,2},{1,3},{2,6},{2,9}};
         int[][] test7exp = {{1,2},{1,3}};
         System.out.println("Test 7: expected=" + matrixToString(test7exp) +
         " actual=" + matrixToString(findClosestRecursive(test7in)));

        // Test task 3
        int[][] test8in = { { 9, 5 }, { 2, 9 }, { 2, 6 }, { 8, 6 }, { 1, 2 },
                { 1, 3 }, { 8, 9 }, { 0, 7 }, { 5, 9 }, { 9, 8 } };
        int[][] test8exp = { { 1, 2 }, { 1, 3 } };
        System.out.println("Test 8: expected=" + matrixToString(test8exp)
                + " actual=" + matrixToString(findClosest(test8in)));
    }
}
