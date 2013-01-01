//package company;

import java.lang.Comparable;
import java.io.*;
import java.util.*;


public class EquipmentPackage implements Comparable<EquipmentPackage> {

    final private String type;
    private int amount;
    final private int price;  // Relevant only in store.


    public EquipmentPackage(String type, int amount, int price) {
        this.type = new String(type);
        this.amount = amount;
        this.price = price;
    }


    // Sort by amount then expensive price.
    public int compareTo(EquipmentPackage p) {
        if (this.amount == p.getAmount()) {
            return - (this.price - p.getPrice());  // Expensive appears before cheap (we reverse after sort).
        } else {
            return this.amount - p.getAmount();
        }
    }


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
        String NEW_LINE = System.getProperty("line.separator");

        result.append(NEW_LINE + "______________________________________" + NEW_LINE);
        result.append("           ---Equipment Packages For Sale---: " + NEW_LINE);
        result.append("Type: " + this.type + " ");
        result.append("Amount: " + this.amount + " ");
        result.append("Price: " + this.price);
        return result.toString();
    }
}
