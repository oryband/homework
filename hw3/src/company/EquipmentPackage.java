package company;

import java.io.*;
import java.util.*;
import utilities.*;

public class EquipmentPackage {

    private String type;
    private int amount;
    private int price; // Relevant only in store
    
    // Constructor
    public EquipmentPackage(String equipmentType,
                            int equipmentAmount, 
                            int price) {
        this.type = new String(equipmentType);
        this.amount = equipmentAmount; 
        this.price = price;

    }
    
   public String toString(){

        StringBuilder result = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");

        result.append(NEW_LINE + "Type: " + this.type + " ");
        result.append("Amount: " + this.amount + " ");
        result.append("Price: " + this.price);
        return result.toString();
    }
}
