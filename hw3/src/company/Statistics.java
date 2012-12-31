//package company;

import java.io.*;
import java.util.*;


public class Statistics {

    private int budget;
    private int rewards;
    private int moneySpent;

    private ArrayList<Scientist> scientistsPurchased;
    private ArrayList<EquipmentPackage> equipmentPackagesPurchased;
    private ArrayList<Laboratory> labsPurchased;


    public Statistics(int budget) {
        this.budget = budget;
        this.rewards = 0 ;
        this.moneySpent = 0;
        this.scientistsPurchased = null;
        this.equipmentPackagesPurchased = null;
        this.labsPurchased = null;
    }

    public Statistics() {
        this(0);
    }

    // Setters
    // TODO Remove this, and set budget in constructor (used in util).
    public void setBudget(int i) {
        this.budget = i;
    }

    public void chargePrice(int price) {
        this.budget -= price;
        this.moneySpent += price;
    }
    
    public void addReward(int reward) {
        this.budget += reward;
        this.rewards += reward;
    }


    public int getBudget() {
        return this.budget;
    }


    public String toString() {

        StringBuilder result = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");

        result.append("______________________________________" + NEW_LINE);
        result.append("           ---SATISTICS---: " + NEW_LINE);
        result.append("Budget: " + this.budget + NEW_LINE);
        return result.toString();
    }
}
