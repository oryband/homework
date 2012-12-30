package company;

import java.lang.Comparable;
import java.io.*;
import java.util.*;


public class EquipmentPackage implements Comparable<EquipmentPackage> {

    final private String type;
    final private int amount;
    final private int price;  // Relevant only in store.


    public EquipmentPackage (
            String equipmentType,
            int equipmentAmount, 
            int price) {

        this.type = new String(equipmentType);
        this.amount = equipmentAmount; 
        this.price = price;
    }


    public int compareTo(EquipmentPackage p) {
        return this.amount - p.getEquipmentAmount();
    }


    public int getEquipmentAmount() {
        return this.amount;
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
