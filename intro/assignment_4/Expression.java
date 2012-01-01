/**
 * Represents an expression between variables.
 *
 * @author Ory Band
 * @version 1.0
 */
public interface Expression {
    /**
     * @param assignments The assignments to evaluate.
     *
     * @return the evaluated value.
     */
    public double evaluate(Assignments assignments);

    /**
     * @param var The variable to deriviate.
     *
     * @return the value's derivative.
     */
    public Expression derivative(Variable var);
}

