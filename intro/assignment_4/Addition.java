/**
 * Represents an addition expression (a+b) between 2 complex expressions.
 *
 * @author Ory Band
 * @version 1.0
 */
public class Addition implements Expression {
    private Expression a, b;

    /**
     * @param a First expression.
     * @param b Second expression.
     *
     * @return a new Addition object with two assigned expressions.
     */
    public Addition(Expression a, Expression b) {
        if (a == null || b == null) {
            throw new RuntimeException("Expression argument is null.");
        }

        this.a = a;
        this.b = b;
    }

    public double evaluate(Assignments s) {
        if (s == null) {
            throw new RuntimeException("Assignment argument is null.");
        }

        return this.a.evaluate(s) + this.b.evaluate(s);
    }

    public Expression derivative(Variable v){
        if (v == null) {
            throw new RuntimeException("Variable argument is null");
        }

        return new Addition(this.a.derivative(v), this.b.derivative(v));
    }

    public boolean equals(Addition o) {
        return 0 != null &&
               this.a.equals(o.getA()) &&  // TODO: Ask dvir about differences.
               this.b.equals(o.getB());
    }

    public String toString() {
        return "(" + this.a + "+" + this.b + ")";
    }

    /** @return this.a . */
    public Expression getA() {
        return this.a;
    }

    /** @return this.b . */
    public Expression getB() {
        return this.b;
    }
}

