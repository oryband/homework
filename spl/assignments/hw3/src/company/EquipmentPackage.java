/** @author Eldar Damari, Ory Band */

package company;

import java.lang.Comparable;


public class EquipmentPackage implements Comparable<EquipmentPackage> {

    final private String type;
    private int amount;
    final private int price;  // Relevant only in store.


    public EquipmentPackage(String type, int amount, int price) {
        this.type = new String(type);
        this.amount = amount;
        this.price = price;
    }


    /**
     * Sorts by amount, then by price - from most expensive to cheapest.
     *
     * @param p equipment package to compare against.
     */
    public int compareTo(EquipmentPackage p) {
        if (this.amount == p.getAmount()) {
            return - (this.price - p.getPrice());
        } else {
            return this.amount - p.getAmount();
        }
    }


    // Getters.
    public String getType() {
        return this.type;
    }

    public int getAmount() {
        return this.amount;
    }

    public int getPrice() {
        return this.price;
    }


    public String toString() {

        StringBuilder result = new StringBuilder();
        String N = System.getProperty("line.separator");

        result.append(N);
        result.append(this.amount + " ");
        result.append(this.type + "s ");
        result.append(this.price + "$");

        return result.toString();
    }
}
