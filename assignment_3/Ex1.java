/***************************************************
 * intro121/ipis121: Third assignment              *
 *                                                 *
 * This class is for assignment #3 - Part 1        *
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

public class Ex1 {

	/******************** Task 1 ********************/
	public static long tilesPack1a(int n) {
		long res = 0;
		// YOUR CODE HERE
		return res;
	}
	
	public static long tilesPack1b(int n) {
		long[] mem = new long[n + 1];
		// INITIALIZE THE LOOKUPTABLE mem
		return tilesPack1b(n, mem);
	}
	
	public static long tilesPack1b(int n, long[] mem) {
		long res = 0;
		// YOUR CODE HERE
		return res;
	}

	/******************** Task 2 ********************/
	public static long tilesPack2(int n) {
		long res = 0;
		// YOUR CODE HERE
		return res;
	}
	
	/******************** Task 3 ********************/
	public static long tilesPack3(int n) {
		long res = 0;
		// YOUR CODE HERE
		return res;
	}

	public static void main(String[] args) {
		// Test task 1a
		System.out.println("Test 1: expected=" + 1 + " actual=" + tilesPack1a(0));
		System.out.println("Test 2: expected=" + 2 + " actual=" + tilesPack1a(2));
		System.out.println("Test 3: expected=" + 5 + " actual=" + tilesPack1a(4));
		
		// Test task 1b
		System.out.println("Test 4: expected=" + 5 + " actual=" + tilesPack1b(4));
		System.out.println("Test 5: expected=" + 225851433717L + " actual=" + tilesPack1b(55));
		
		// Test task 2
		System.out.println("Test 6: expected=" + 1 + " actual=" + tilesPack2(1));
		System.out.println("Test 7: expected=" + 4 + " actual=" + tilesPack2(2));
		System.out.println("Test 8: expected=" + 9 + " actual=" + tilesPack2(3));
		
		// Test task 3
		System.out.println("Test 9: expected=" + 4 + " actual=" + tilesPack3(2));
		System.out.println("Test 10: expected=" + 16 + " actual=" + tilesPack3(3));
		System.out.println("Test 11: expected=" + 49 + " actual=" + tilesPack3(4));
	}

}
