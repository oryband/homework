/**
 * This is a testing framework. Use it extensively to verify that your code is working
 * properly.
 */
public class Tester {

	private static boolean testPassed = true;
	private static int testNum = 0;
	
	/**
	 * This entry function will test all classes created in this assignment.
	 * @param args command line arguments
	 */
	public static void main(String[] args) {
		
		// Each function here should test a different class.
		testPoint();
		testTriangle();
		/* TODO - write a function for each class */
		
		// Notifying the user that the code have passed all tests. 
		if (testPassed) {
			System.out.println("All " + testNum + " tests passed!");
		}
	}

	/**
	 * This utility function will count the number of times it was invoked. 
	 * In addition, if a test fails the function will print the error message.  
	 * @param exp The actual test condition
	 * @param msg An error message, will be printed to the screen in case the test fails.
	 */
	private static void test(boolean exp, String msg) {
		testNum++;
		
		if (!exp) {
			testPassed = false;
			System.out.println("Test " + testNum + " failed: "  + msg);
		}
	}
	
	/**
	 * Checks the Point class.
	 */
	private static void testPoint() {
		
		Point p1 = new Point(10, 20);

		test(p1.getX() == 10, "X should be 10.");
		test(p1.getY() == 20, "Y should be 20.");
		
		Point p2 = new Point(p1);
		test(p2.equals(p1), "Point should be equal.");
		test(p2 != p1, "Point should be different points.");
		test(p2.distance(p1) == 0, "Distance should be zero.");
		
		p2.move(p1);
		test(p2.getX() == 2*p1.getX(), "P2 x should be twice as P1 x.");
		test(p2.getY() == 2*p1.getY(), "P2 y should be twice as P1 y.");
		test(p2.distance(p1) == 22.360679774997898, "Distance should be 22.360679774997898");
		test(!p2.equals(p1), "Point should not be equal.");
		
		/* TODO add more tests to the Point class! */
	}

	/**
	 * Checks the Triangle class.
	 */
	private static void testTriangle() {
		
		/* TODO Go for it! write your tests here for the Triangle class! */
	}
}