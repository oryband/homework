/**
 * Represents a Variable of real (R) type.
 *
 * @author Ory Band
 * @version 1.0
 */
public class VariableExpression implements Variable, Expression {
    private char name;

    /**
     * @param name Variable's name.
     *
     * @return a new VariableExpression object with an initialized name.
     */
    public VariableExpression(char name) {
        this.name = name;
    }

    public char getName() {
        return this.name;
    }

    public double evaluate(Assignments s) {
        if (s == null) {
            throw new RuntimeException("Assigments argument is null.");
        }

        return s.valueOf(this);
    }

    public Expression derivative(Variable v) {
        if (v == null) {
            throw new RuntimeException("Variable argument is null.");
        }

        if (this.equals(v)) {
            return new Constant(1);
        } else {
            return new Constant(0);
        }
    }

    /**
     * @param other Object to be compared.
     *
     * @return True if Variable isn't null, and if its name is equal to the argument object's name.
     */
    public boolean equals(VariableExpression v) {
        return v != null && this.getName() == v.getName();
    }

    /** @return Variable's name. */
    public String toString() {
        return "" + this.name;
    }
}
