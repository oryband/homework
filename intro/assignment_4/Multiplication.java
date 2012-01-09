/**
 * Represents a multiplication (a*b) expression between 2 complex expressions.
 *
 * @author Ory Band
 * @version 1.0
 */
public class Multiplication implements Expression {
    private Expression a, b;

    /**
     * @param a First expression.
     * @param b Second expression.
     *
     * @return a new Multiplication object with two assigned expressions.
     */
    public Multiplication(Expression a, Expression b) {
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

        return this.a.evaluate(s) * this.b.evaluate(s);
    }

    public Expression derivative(Variable v){
        if (v == null) {
            throw new RuntimeException("Variable argument is null");
        }

        return new Addition(
                new Multiplication(this.a,               this.b.derivative(v)),
                new Multiplication(this.a.derivative(v), this.b));
    }

    public boolean equals(Multiplication m) {
        return m != null &&
               this.a.equals(m.getA()) &&
               this.b.equals(m.getB());
    }

    public String toString() {
        return "(" + this.a + "*" + this.b + ")";
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

