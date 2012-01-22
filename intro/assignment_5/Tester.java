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
		testQuadrangle();
        testShapesContainer();
		
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

        Point p3 = new Point(p1.getX(), p1.getY());
        test(p3.equals(p1), "p3 should be equal to p1.");

        Point p4 = new Point(10, 20);
        test(p4.equals(p1), "p4 should be equal to p1.");
        test(p4.equals(p3), "p4 should be equal to p3.");

        Point p5 = new Point(0, 0);
        p5.move(p1.getX(), p1.getY());
        test(p5.equals(p1), "p5, should be equal to p1.");
	}

	/**
	 * Checks the Triangle class.
	 */
	private static void testTriangle() {
        Point p1 = new Point(0,0),
              p2 = new Point(2,0),
              p3 = new Point(1,2);

        Triangle t1 = new Triangle(p1, p2, p3),
                 t2 = new Triangle(t1),
                 t3 = new Triangle(p2, p1, p3);


        test(t1.getP1().equals(new Point(0,0)), "t1.p1 should be (0,0)");
        test(t1.getP2().equals(new Point(2,0)), "t1.p2 should be (2,0)");
        test(t1.getP3().equals(new Point(1,2)), "t1.p3 should be (1,2)");

        test(t1.getNumOfPoints() == 3 && t2.getNumOfPoints() == 3 && t3.getNumOfPoints() == 3, "t1/2/3.pi count should be 3.");

        test(t1.equals(t2) && t2.equals(t3), "t1 != t2 != t3");

        test(t1.getP1().equals(t2.getP1()), "t1.p1 != t2.p1");
        test(t1.getP2().equals(t2.getP2()), "t1.p2 != t2.p2");
        test(t1.getP3().equals(t2.getP3()), "t1.p3 != t2.p3");

        test(t1.getP1().equals(t3.getP2()), "t1.p1 != t3.p2");
        test(t1.getP2().equals(t3.getP1()), "t1.p2 != t3.p1");
        test(t1.getP3().equals(t3.getP3()), "t1.p3 != t3.p3");

        test(t1.getPoints()[0].equals(t2.getPoints()[0]), "t1/2 1st points aren't equal.");
        test(t1.getPoints()[1].equals(t2.getPoints()[1]), "t1/2 2nd points aren't equal.");
        test(t1.getPoints()[2].equals(t2.getPoints()[2]), "t1/2 3rd points aren't equal.");

        test(t1.getPerimeter() == t2.getPerimeter() && t2.getPerimeter() == t3.getPerimeter(), "t1,t2,t3's perimeter should be equal.");
        test(t1.getArea() == t2.getArea() && t2.getArea() == t3.getArea(), "t1,t2,t3's area should be equal.");

        Point p4 = new Point(1.5,1);
        test(t1.contains(p4) && t2.contains(p4) && t3.contains(p4), "p4 should be in t1, t2, t3.");
	}

	/**
	 * Checks the Quadrangle class.
	 */
	private static void testQuadrangle() {
        Point p1 = new Point(0,0),
              p2 = new Point(1,0),
              p3 = new Point(1,1),
              p4 = new Point(0,1);

        Quadrangle q1 = new Quadrangle(p1, p2, p3, p4),
                   q2 = new Quadrangle(q1),
                   q3 = new Quadrangle(p2, p3, p4, p1);


        test(q1.getP1().equals(new Point(0,0)), "q1.p1 should be (0,0)");
        test(q1.getP2().equals(new Point(1,0)), "q1.p2 should be (1,0)");
        test(q1.getP3().equals(new Point(1,1)), "q1.p3 should be (0,1)");
        test(q1.getP4().equals(new Point(0,1)), "q1.p4 should be (1,1)");

        test(q1.getNumOfPoints() == 4 && q2.getNumOfPoints() == 4 && q3.getNumOfPoints() == 4, "q1/2/3.pi count should be 4.");

        test(q1.equals(q2) && q2.equals(q3), "q1 != q2 != q3");

        test(q1.getP1().equals(q2.getP1()), "q1.p1 != q2.p1");
        test(q1.getP2().equals(q2.getP2()), "q1.p2 != q2.p2");
        test(q1.getP3().equals(q2.getP3()), "q1.p3 != q2.p3");
        test(q1.getP4().equals(q2.getP4()), "q1.p4 != q2.p4");

        test(q1.getP1().equals(q3.getP4()), "q1.p1 != q3.p4");
        test(q1.getP2().equals(q3.getP1()), "q1.p2 != q3.p1");
        test(q1.getP3().equals(q3.getP2()), "q1.p3 != q3.p2");
        test(q1.getP4().equals(q3.getP3()), "q1.p4 != q3.p3");

        test(q1.getPoints()[0].equals(q2.getPoints()[0]), "q1/2 1st points aren't equal.");
        test(q1.getPoints()[1].equals(q2.getPoints()[1]), "q1/2 2nd points aren't equal.");
        test(q1.getPoints()[2].equals(q2.getPoints()[2]), "q1/2 3rd points aren't equal.");
        test(q1.getPoints()[3].equals(q2.getPoints()[3]), "q1/2 4th points aren't equal.");

        test(q1.getPerimeter() == 4, "q1 perimeter should be 4.");
        test(q1.getArea() >= 0.9 && q1.getArea() <= 1.1, "q1 area should be 1.");

        test(q1.getPerimeter() == q2.getPerimeter() && q2.getPerimeter() == q3.getPerimeter(), "q1,q2,q3's perimeter should be equal.");
        test(q1.getArea() == q2.getArea() && q2.getArea() == q3.getArea(), "q1,q2,q3's area should be equal.");

        Point p5 = new Point(0.5,0.5);
        test(q1.contains(p4) && q2.contains(p4) && q3.contains(p4), "p5 should be in q1, q2, q3.");
	}

	private static void testShapesContainer() {
		ShapesContainer c = new ShapesContainer();

        test(c.getShapesNum() == 0, "Container should be empty.");

        Triangle t1 = new Triangle(new Point(0,0),
                                   new Point(2,0),
                                   new Point(1.5,2));
        c.add(t1);
        test(!c.add(t1), "t1 was added twice in a row!");
        test(c.getShapesNum() == 1, "Container should be of length 1.");

        Triangle t2 = new Triangle(new Point(0,0),
                                   new Point(5,0),
                                   new Point(3,5));
        c.add(t2);

        Quadrangle q1 = new Quadrangle(new Point(0,0),
                                       new Point(1,0),
                                       new Point(1,1),
                                       new Point(0,1));
        c.add(q1);

        ShapesContainer c2 = new ShapesContainer(c);

        test(c.sumPerimeter() > 26 && c.sumPerimeter() < 27, "Sum of perimeters is wrong.");
        test(c.sumArea() > 15 && c.sumArea() < 16, "Sum of areas is wrong.");

        test(c.getShapesNum() == 3, "Container should be of length 2.");
        c.remove(0);
        c.remove(t2);
        test(c.getShapesNum() == 2, "Container should be of length 1.");
        c.remove(t1);
        test(c.getShapesNum() == 1, "Container should be empty.");

        test(c.sumPerimeter() > 3 && c.sumPerimeter() < 5, "Sum of perimeters is wrong.");
        test(c.sumArea() > 0 && c.sumArea() < 2, "Sum of areas is wrong.");

        c2.move(new Point(1,1));

        test(c2.sumPerimeter() > 26 && c2.sumPerimeter() < 27, "Sum of perimeters 2 is wrong.");
        test(c2.sumArea() > 15 && c2.sumArea() < 16, "Sum of areas 2 is wrong.");

        test(c2.getShapesNum() == 3, "Container 2 should be of length 2.");
        c2.remove(0);
        test(c2.getShapesNum() == 2, "Container 2 should be of length 1.");
        c2.remove(t1);
        test(c2.getShapesNum() == 1, "Container 2 should be empty.");
	}
}
