/**
 * @author Ory Band
 * @version 1.0
 */
public class ValueAssignment implements Assignment {
    Variable var;
    double value;

    /**
     * Assign initial values.
     *
     * @param var Variable object.
     * @param value Variable's value.
     *
     * @return New ValueAssignment object.
     */
    public ValueAssignment(Variable var, double value) {
        this.var   = var;
        this.value = value;
    }

    /** @return True if var instances and their values are equal. */
    public boolean equals(ValueAssignment other) {
        return this.var.equals(other.var) && this.value == other.value;
    }

    /** @return string in format "name=value" . */
    public String toString() {
        return var.getName + "=" + value;
    }
}

