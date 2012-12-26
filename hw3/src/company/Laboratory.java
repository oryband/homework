package company;

import java.io.*;
import java.util.*;


public class Laboratory {

    private String name;
    private String specialization;
    private int numberOfScientists;
    private int price;


    public Laboratory(String headLabName,
                      String special,
                      int numOfScientists,
                      int price) {
        this.name = new String(headLabName);
        this.specialization = new String(special);
        this.numberOfScientists = numOfScientists;
        this.price = price;
    }
    
    public String toString(){

        StringBuilder result = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");
        
        result.append("______________________________________" + NEW_LINE);
        result.append("           ---Laboratory For Sale---: " + NEW_LINE);
        result.append("Name: " + this.name + NEW_LINE);
        result.append("Specialization: " + this.specialization + NEW_LINE);
        result.append("Number Of Scientists: " + this.numberOfScientists + NEW_LINE);
        result.append("Price: " + this.price + NEW_LINE);
        return result.toString();
    }
}
