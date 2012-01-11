/**
 * @author Ory Band
 * @version 1.0
 */
public class ValueAssignment implements Assignment {
    private Variable var;
    private double   value;

    /**
     * Assign initial values.
     *
     * @param var Variable object.
     * @param value Variable's value.
     *
     * @return New ValueAssignment object.
     */
    public ValueAssignment(Variable var, double val) {
        if (var == null) {
            throw new RuntimeException("Variable argument is null.");
        }

        this.var   = var;
        this.value = val;
    }

    public Variable getVar() {
        return this.var;
    }

    public double getValue() {
        return this.value;
    }

    public void setValue(double value) {
        this.value = value;
    }

    /**
     * @param o Object to be compared.
     *
     * @return True if object is of type ValueAssignment and if Variable instances and their values are equal. */
    public boolean equals(Object o) {
        return o instanceof ValueAssignment &&
               this.var.equals( ((ValueAssignment) o) .getVar()) &&
               this.value ==    ((ValueAssignment) o) .getValue();
    }

    /** @return string in format "name=value" . */
    public String toString() {
        return this.var.getName() + "=" + this.value;
    }
}

