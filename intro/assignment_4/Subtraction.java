/**
 * Represents a substraction expression (a-b) between 2 complex expressions.
 *
 * @author Ory Band
 * @version 1.0
 */
public class Substraction implements Expression {
    private Expression a, b;

    /**
     * @param a First expression.
     * @param b Second expression.
     *
     * @return a new Substraction object with two assigned expressions.
     */
    public Substraction(Expression a, Expression b) {
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

        return this.a.evaluate(s) - this.b.evaluate(s);
    }

    public Expression derivative(Variable v){
        if (v == null) {
            throw new RuntimeException("Variable argument is null");
        }

        return new Substraction(this.x.derivative(v), this.y.derivative(v));
    }

    public boolean equals(Substraction o) {
        return 0 != null &&
               this.x.equals(o.x) &&  // TODO: Ask dvir about differences.
               this.y.equals(o.y);
    }

    public String toString() {
        return "(" + this.x + "-" + this.y + ")";
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

