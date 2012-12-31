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


    public int compareTo(EquipmentPackage p) {
        return this.amount - p.getAmount();
    }


    public void decrementAmount() {
        this.amount --;
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

        result.append(NEW_LINE + "Type: " + this.type + " ");
        result.append("Amount: " + this.amount + " ");
        result.append("Price: " + this.price);
        return result.toString();
    }
}
