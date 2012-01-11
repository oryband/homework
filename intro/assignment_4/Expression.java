/**
 * Represents an expression between variables.
 *
 * @author Ory Band
 * @version 1.0
 */
public interface Expression {
    /**
     * @param assignments The assignments listst to evaluate the variable against.
     *
     * @return the Variable's value according to the assignments list.
     */
    public double evaluate(Assignments assignments);

    /**
     * @param var The variable to deriviate.
     *
     * @return the value's derivative.
     */
    public Expression derivative(Variable var);
}

