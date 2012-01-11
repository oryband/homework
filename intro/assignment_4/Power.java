/**
 * Represents a power (a**b or a^b) expression between 2 complex expressions.
 *
 * @author Ory Band
 * @version 1.0
 */
public class Power implements Expression {
    private Expression base;
    private double     exponent;

    /**
     * @param base Power's base.
     * @param ex Power's Exponent.
     *
     * @return a new Power object with two assigned expressions.
     */
    public Power(Expression base, double ex) {
        if (base == null) {
            throw new RuntimeException("Expression argument is null.");
        }

        this.base     = base;
        this.exponent = ex;
    }

    public double evaluate(Assignments s) {
        if (s == null) {
            throw new RuntimeException("Assignment argument is null.");
        }

        //return this.base.evaluate(s) ** this.exponent.evaluate(s);
        return Math.pow(this.base.evaluate(s), this.exponent);
    }

    public Expression derivative(Variable v){
        if (v == null) {
            throw new RuntimeException("Variable argument is null");
        }

        double ex = this.exponent -1;

        if (ex <= 0) {
            return new Constant(0);
        } else {
            // d[a^b] = d[a]*(b*(a^b-1))
            return new Multiplication(
                        new Multiplication(new Constant(this.exponent), new Power(this.base, ex)),
                        this.base.derivative(v));
        }
    }

    /**
     * @param o Object.
     *
     * @return True if object is of type Power and if base and exponent are equal.
     */
    public boolean equals(Object o) {
        return o instanceof Power  &&
               this.base.equals( ((Power) o) .getBase()) &&
               this.exponent ==  ((Power) o) .getExponent();
    }

    public String toString() {
        return "(" + this.base + "^" + this.exponent + ")";
    }

    /** @return this.base . */
    public Expression getBase() {
        return this.base;
    }

    /** @return this.exponent . */
    public double getExponent() {
        return this.exponent;
    }
}

