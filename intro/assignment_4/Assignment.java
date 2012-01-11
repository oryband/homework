/**
 * Represents a variable's assignment.
 *
 * Thus, a class implementing this interface,
 * represents a variable and its value (type double).
 *
 * @author Ory Band
 * @version 1.0
 */
public interface Assignment {
    /** @return the variable. */
    public Variable getVar();

    /** @return the variable's value. */
    public double getValue();

    /** Sets the variable's value. */
    public void setValue(double value);
}

