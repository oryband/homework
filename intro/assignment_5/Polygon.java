/**
 * Represents a generic polygon shape.
 *
 * @author Ory Band
 * @version 1.0
 */
public abstract class Polygon implements Shape {
    protected Point[] points;

    /**
     * @param p Coordinate list.
     *
     * @return a new initialized Polygon object.
     */
    public Polygon(Point[] ps) {
        // Validity tests.
        if (p == null) {
            throw new RuntimeException("Point argument is null.");
        } else if (p.length < 3) {
            throw new RuntimeException("Not enough points for Polygon (Minimum 3) - Point[] is too short.");
        }

        this.points = new Point[p.length];

        for (int i=0; i<p.length; i++) {
            if (p == null) {
                throw new RuntimeException("Point[" + i + "] is null.");
            } else {
                this.points[i] = new Point(p.getX(), p.getY());
            }
        }
    }

    /** @return amount of Polygon's coordinates. */
    public int getNumOfPoints() {
        return this.points.length;
    }

    /** @return sides' list. */
    public double[] getSides() {
        int l = this.points.length;

        double[] sides = new double[l];

        double x1, x2,
               y1, y2;

        // Iterate over points and calculate sides.
        for (int i=0; i<l; i++) {
            x1 = this.points[i].getX();
            y1 = this.points[i].getY();

            // If we reached the last coord, calculate side against first point.
            if (i == l-1) {
                x2 = this.points[0].getX();
                y2 = this.points[0].getY();
            } else {
                x2 = this.points[i+1].getX();
                y2 = this.points[i+1].getY();
            }

            // Calculate distance between 2 points.
            sides[i] = Math.sqrt(Math.pow(x2-x1, 2) + Math.pow(y2-y1, 2));
        }

        return perimeter;

    }

    /** @return Coordinate list. */
    public Point[] getPoints() {
        return this.points;
    }

    public double getPerimeter() {
        // Get sides length;
        double[] sides = this.getSides();

        // Sum sides.
        double perimeter = 0.0;
        for (int i=0; i<sides.length; i++) {
            perimeter += sides[i];
        }

        return perimeter;
    }

    public void move(Point p) {
        // Validity test.
        if (p == null) {
            throw new RuntimeException("Point argument is null.");
        }

        // Shift coordinates according to point, given as argument.
        for (int i=0; i<this.points.length; i++) {
            this.points[i].setX(this.points[i].getX() + p.getX());
            this.points[i].setY(this.points[i].getY() + p.getY());
        }
    }

    public double getArea();
    public boolean contains(Point p);
}
