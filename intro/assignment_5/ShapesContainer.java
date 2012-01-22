/**
 * Represents a group of Shapes.
 *
 * @author Ory Band
 * @version 1.0
 */
public class ShapesContainer {
    public static final int INIT_SIZE = 10;  // Default size for empty constructor.
    public static final int RESIZE    = 10;  // Extension size when list is full.

    private Shape[] shapes;

    /** @return a new initializes ShapesContainer[] object with a default size. */
    public ShapesContainer() {
        this.shapes = new Shape[INIT_SIZE];
    }

    /**
     * @param s Shape list.
     *
     * @return a new initializes ShapesContainer[] object with an empty Shapes[] list.
     */
    public ShapesContainer(ShapesContainer s) {
        // Validity test.
        if (s == null) {
            throw new RuntimeException("ShapesContainer argument is null.");
        }

        int l = s.getShapesNum();
        //this.shapes = new Shape[l + l % this.RESIZE];  // Init new object with size rounded up by INIT_SIZE.
        this.shapes = new Shape[l];  // Assignment paper says to shallow copy.

        for (int i=0; i<l; i++) {
            this.add(s.getShape(i));
        }
    }

    /**
     * Inserts shape ordered by area (largest first).
     *
     * @param s Shape to add.
     *
     * @return true if shape was added successfully, otherwise false (e.g. was already in container).
     */
    public boolean add(Shape s) {
        // Validity test.
        if (s == null) {
            //throw new RuntimeException("Shape argument is null.");
            return false;  // Forum says to return false instead of throwing exceptions for invalid arguments in add/remove.
        }

        // Search for insert index. 
        double a = s.getArea();
        int i,
            insert = -1;  // Insert index.
        for (i=0; i < this.getShapesNum(); i++) {
            if (this.shapes[i].getArea() > a) {
                insert = i;
            }

            // Check for duplicates.
            if (this.shapes[i].equals(s)) {
                return false;
            }
        }

        // Resize if container is full.
        if (this.getShapesNum() == this.shapes.length) {
            Shape[] newShapes = new Shape[this.shapes.length + this.RESIZE];

            for (i=0; i < this.shapes.length; i++) {
                newShapes[i] = this.shapes[i];
            }

            this.shapes = newShapes;
        }

        // Please new shape in its place, ordered by size.
        for (i = this.getShapesNum() -1; i > insert; i--) {
            this.shapes[i+1] = this.shapes[i];
        }

        this.shapes[insert +1] = s;

        return true;
    }


    /**
     * @param i Shape index to remove.
     *
     * @return true if shape was found and removed, otherwise false.
     */
    public boolean remove(int i) {
        if (i<0 || i >= this.shapes.length || this.shapes[i] == null) {
            //throw new RuntimeException("Index argument out of bounds.");
            return false;  // Forum says to return false instead of throwing exceptions for invalid arguments in add/remove.
        //} else if (this.shapes[i] == null) {
            //throw new RuntimeException("Shape[" + i +"] is null.");
            //return false;  // Forum says to return false instead of throws exceptions for invalid arguments in add/remove.
        }

        // Shrink container.
        for (; i < this.shapes.length -2; i++) {
            this.shapes[i] = this.shapes[i+1];
        }

        // Removed the last duplicate shape.
        this.shapes[this.shapes.length -1] = null;

        return true;
    }

    /**
     * @param s Shape to search for and remove.
     *
     * @return true if shape was found and removed, otherwise false.
     */
    public boolean remove(Shape s) {
        // Validity test.
        if (s == null) {
            //throw new RuntimeException("Shape argument is null.");
            return false;  // Forum says to return false instead of throwing exceptions for invalid arguments in add/remove.
        }

        // Search for shape.
        for (int i=0; i<this.shapes.length; i++) {
            //if (this.shapes[i].equals(s)) {  // Logic comparison.
            if (this.shapes[i] == s) {  // FAQ says to use pointer comparison.
                return this.remove(i);
            }
        }

        // Fail if shape wasn't found.
        return false;
    }

    /** @return Size of shapes group. */
    public int getShapesNum() {
        int s=0;

        // Count shapes.
        for (int i=0; i<this.shapes.length; i++) {
            // Container is well ordered, so null means we reached the end of the list.
            if (this.shapes[i] == null) {
                return s;
            } else {
                s++;
            }
        }

        return s;
    }

    /**
     * @param i index of shape to return
     * 
     * @return Shape object according to idnex received as argument.
     */
    public Shape getShape(int i) {
        if (i<0 || i >= this.shapes.length) {
            throw new RuntimeException("Index argument out of range.");
        }

        return this.shapes[i];
    }

    /** @return Sum of all shapes' areas. */
    public double sumArea() {
        double areas = 0.0;

        for (int i=0; i<this.getShapesNum(); i++) {
            areas += this.shapes[i].getArea();
        }

        return areas;
    }

    /** @return Sum of all shapes' perimeters. */
    public double sumPerimeter() {
        double perimeters = 0.0;

        for (int i=0; i<this.getShapesNum(); i++) {
            perimeters += this.shapes[i].getPerimeter();
        }

        return perimeters;
    }

    /**
     * @param p Point coordinate to shift all shapes by.
     *
     * @return Sum of all shapes' perimeters.
     */
    public void move(Point p) {
        if (p == null) {
            throw new RuntimeException("Point argument is null.");
        }

        for (int i=0; i<this.shapes.length; i++) {
            this.shapes[i].move(p);
        }
    }
}

