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

    public void setVaule(double value) {
        this.value = value;
    }

    /**
     * @param o Object to be compared.
     *
     * @return True if Varable instances and their values are equal. */
    public boolean equals(ValueAssignment o) {
        return o != null &&
               this.var.equals(o.getVar()) &&
               this.value == o.getValue();
    }

    /** @return string in format "name=value" . */
    public String toString() {
        return this.var.getName() + "=" + this.value;
    }
}

