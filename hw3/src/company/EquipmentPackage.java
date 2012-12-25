package company;

import java.io.*;
import java.util.*;
import utilities.*;

public class EquipmentPackage {

    private String type;
    private int amount;

   public EquipmentPackage(String equipmentType,
                     int equipmentAmount) {
        this.type = new String(equipmentType);
        this.amount = equipmentAmount; 

    }
    
   public String toString(){

        StringBuilder result = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");

        result.append("Type: " + type + NEW_LINE);
        result.append("Amount: " + amount + NEW_LINE);
        return result.toString();
    }
}
