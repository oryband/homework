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
		int[][] res = null;
		// YOUR CODE HERE
		return res;
	}

	/******************** Task 2 ********************/
	public static int[][] sortByLex(int[][] points) {
		int[][] ans = null;
		// YOUR CODE HERE
		return ans;
	}

	public static int[][] sortByY(int[][] points) {
		int[][] ans = null;
		// YOUR CODE HERE
		return ans;
	}

	public static int[] duplicates(int[][] points) {
		int[] res = null;
		// YOUR CODE HERE
		return res;
	}

	public static int[][] filterPointsByRange(double fromX, double toX,
			int[][] points) {
		int[][] res = null;
		// YOUR CODE HERE
		return res;
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
