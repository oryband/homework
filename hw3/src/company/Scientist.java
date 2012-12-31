//package company;

import java.lang.Comparable;
import java.io.*;
import java.util.*;


public class Scientist implements Comparable<Scientist> {

    final private String name;
    final private String specialization;
    final private int price;


    public Scientist(String name, String specialty, int price) {
        this.name = name;
        this.specialization = specialization;
        this.price = price;
    }


    public int compareTo(Scientist s) {
        return this.price - s.getPrice();
    }


    public String getSpecialization() {
        return this.specialization;
    }

    public int getPrice() {
        return this.price;
    }


    public String toString() {

        StringBuilder result = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");

        result.append("______________________________________" + NEW_LINE);
        result.append("           ---Scientists For Sale---: " + NEW_LINE);
        result.append("Name: " + this.name + NEW_LINE);
        result.append("Specialization: " + this.specialty + NEW_LINE);
        result.append("Price: " + this.price + NEW_LINE);
        return result.toString();
    }
}
