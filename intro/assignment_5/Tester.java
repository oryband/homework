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
        testCircle();
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

    private static void testCircle() {
        Point p1 = new Point(0,0),
              p2 = new Point(7,7.5);

        double r1 = 1,
               r2 = 2.5;

        Circle c1 = new Circle(p1, r1),
               c2 = new Circle(p2, r2),
               c3 = new Circle(c1);


        test(c1.getCenter().equals(new Point(0,0)), "c1.center should be (0,0)");
        test(c1.getRadius() > 0.9 && c1.getRadius() < 1.1, "c1.radius should be ~1.0");
        test(c2.getCenter().equals(new Point(7,7.5)), "c2.center should be (7,7.5)");
        test(c2.getRadius() > 2.4 && c2.getRadius() < 2.6, "c2.radius should be ~2.5");

        test(c1.equals(c3), "c1 != c3");

        //System.out.println("c1 per: " + c1.getPerimeter());
        //System.out.println("c2 per: " + c2.getPerimeter());
        //System.out.println("c3 per: " + c3.getPerimeter());

        //System.out.println("c1 area: " + c1.getArea());
        //System.out.println("c2 area: " + c2.getArea());
        //System.out.println("c3 area: " + c3.getArea());

        test(c1.getPerimeter() > 6 && c1.getPerimeter() < 7, "c1 perimeter should be ~6.");
        test(c1.getArea() > 3 && c1.getArea() < 4, "c1 area should be ~3.");

        test(c1.getPerimeter() == c3.getPerimeter(), "c1,c3's perimeter should be equal.");
        test(c1.getArea() == c3.getArea(), "c1,c3's area should be equal.");

        Point p3 = new Point(0,0.5);
        test(c1.contains(p3) && ! c2.contains(p3) && c3.contains(p3), "p3 should be in c1 & c3 BUT NOT in c2");
    }

    private static void testShapesContainer() {
        ShapesContainer c1 = new ShapesContainer();

        test(c1.getShapesNum() == 0, "c1 should be empty.");

        Triangle t1 = new Triangle(new Point(0,0),
                                   new Point(2,0),
                                   new Point(1.5,2));

        //System.out.println("t1 perimeter: " + t1.getPerimeter());
        //System.out.println("t1 area: " + t1.getArea());
        //System.out.println();

        c1.add(t1);

        test( ! c1.add(t1), "t1 was added twice in a row!");
        test(c1.getShapesNum() == 1, "c1 should be of length 1.");

        Triangle t2 = new Triangle(new Point(0,0),
                                   new Point(6,0),
                                   new Point(3,6));

        //System.out.println("t2 perimeter: " + t2.getPerimeter());
        //System.out.println("t2 area: " + t2.getArea());
        //System.out.println();

        c1.add(t2);

        test(c1.getShapesNum() == 2, "c1 should be of length 2.");

        Quadrangle q1 = new Quadrangle(new Point(0,0),
                                       new Point(1,0),
                                       new Point(1,1),
                                       new Point(0,1));

        //System.out.println("q1 perimeter: " + q1.getPerimeter());
        //System.out.println("q1 area: " + q1.getArea());
        //System.out.println();

        c1.add(q1);

        test(c1.getShapesNum() == 3, "c1 should be of length 3.");

        ShapesContainer c2 = new ShapesContainer(c1);

        test(c2.getShapesNum() == 3, "c2 should be of length 3.");

        test(c1.sumPerimeter() > 29 && c1.sumPerimeter() < 30, "c1: Sum of perimeters is wrong.");
        test(c1.sumArea() > 21 && c1.sumArea() < 22, "c1: Sum of areas is wrong.");

        c1.remove(0);  // should be t2
        c1.remove(t2); // should do nothing.

        test(c1.getShapesNum() == 2, "c1 should be of length 2.");

        c1.remove(t1);

        test(c1.getShapesNum() == 1, "c1 should be of length 1.");

        test(c1.sumPerimeter() > 3 && c1.sumPerimeter() < 5, "c1, after removal: Sum of perimeters is wrong.");
        test(c1.sumArea() > 0 && c1.sumArea() < 2, "c1, after removal: Sum of areas is wrong.");



        c2.move(new Point(1,1));

        test(c2.sumPerimeter() > 29 && c2.sumPerimeter() < 30, "c2: Sum of perimeters 2 is wrong.");
        test(c2.sumArea() > 21 && c2.sumArea() < 22, "c2: Sum of areas 2 is wrong.");
    }
}
