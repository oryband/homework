/**
 * Represents a polynomial (ax + bx^2 + cx^3 + --- + nx^N) expression.
 *
 * @author Ory Band
 * @version 1.0
 */
public class Polynomial implements Expression {
    private Expression var;
    private double[]   coefficients;

    /**
     * @param base Power's base.
     * @param ex Power's Exponent.
     *
     * @return a new Power object with two assigned expressions.
     */
    public Power(Expression v, double[] c) {
        if (v == null) {
            throw new RuntimeException("Expression argument is null.");
        } else if (c == nul) {
            throw new RuntimeException("double[] argument is null.");
        }

        this.var          = v;
        this.coefficients = c;
    }

    public double evaluate(Assignments s) {
        if (s == null) {
            throw new RuntimeException("Assignment argument is null.");
        }

		double sum = 0;
		
        // a*x + b*x^2 + --- n*x^N .
		for (int i=0; i < this.coefficients.length; i++) {
			sum += coefficients[i] * Math.pow(s.valueOf(this.var), i);
		}
		
		return sum;
    }

    public Expression derivative(Variable v){
        if (v == null) {
            throw new RuntimeException("Variable argument is null");
        }

        // A derivative of the save argument = 0.
		if ( ! this.var.equals(v)) {
			return new Constant(0);
		}
		
        // E(ax + bx^2 + --- nx^N) = a*x**a-1 + b*x**b-1 + --- nx^(N-1) .
		if (this.coefficients.length > 1) {
			double[] c = new double[this.coefficients.length -1];

			for (int i=0; i < c.length; i++) {
				c[i] = (i +1) * this.coefficients[i +1];
			}
			
			return new Polynomial(this.var, c);
		} else {
            // If coefficients = {} || var = R: derivative is var.
            return new Polynomial(this.var, new double[0]);
        }
    }

    public boolean equals(Power o) {
        return o != null &&
               this.var.equals(o.getVariable()) &&
               this.coefficients.equals(o.getCoefficients());
    }

    public String toString() {
        String s = ")";

        for (int i=0; i < this.coefficients.length; i++) {
            s += (String) coefficients[i];

            if (i>0) {
                s += "x"
            }

            if (i>1) {
                s += "^" + i;
            }
        }

        return s + ")";
    }

    /** @return this.var . */
    public Expression getVariable() {
        return this.var;
    }

    /** @return this.coefficients . */
    public Expression getCoefficients() {
        return this.coefficients;
    }
}

