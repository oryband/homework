/**
 * Represents a 2d generic shape.
 *
 * @author Ory Band
 * @version 1.0
 */
public interface Shape {
    /** @return shape's perimeter. */
    public double getPerimeter();

    /** @return shape's area. */
    public double getArea();

    /** @param p shifts shape. */
    public void move(Point p);

    /**
     * @param p coordinate to test against.
     *
     * @return True if point is inside the shape.
     */
    public boolean contains(Point p);
}

