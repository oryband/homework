/** @author Eldar Damari, Ory Band */

package company;

import java.lang.Comparable;


public class Scientist implements Comparable<Scientist> {

    final private String name;
    final private String specialization;
    final private int price;


    public Scientist(String name, String specialization, int price) {
        this.name = name;
        this.specialization = specialization;
        this.price = price;
    }


    /**
     * Sorts by cheapest price.
     *
     * @param s Scientist to compare against.
     */
    public int compareTo(Scientist s) {
        return this.price - s.getPrice();
    }


    // Getters.
    public String getSpecialization() {
        return this.specialization;
    }

    public int getPrice() {
        return this.price;
    }


    public String toString() {

        StringBuilder result = new StringBuilder();
        String N = System.getProperty("line.separator");

        result.append(N);
        result.append(this.price + "$, ");
        result.append(this.name + ", ");
        result.append(this.specialization + N);

        return result.toString();
    }
}
