package company;

import java.io.*;
import java.util.*;


public class Statistics {

    private int budget;
    private int moneyGained;
    private int moneySpent;
    private ArrayList<Scientist> scientistsPurchased;
    private ArrayList<EquipmentPackage> equipmentPackagesPurchased;
    private ArrayList<Laboratory> labsPurchased;
    private ArrayList<Experiment> experiments;


    public Statistics() {
        this.budget = 0;
        this.moneyGained = 0 ;
        this.moneySpent = 0;
        this.scientistsPurchased = new ArrayList<Scientist>();
        this.equipmentPackagesPurchased = new ArrayList<EquipmentPackage>();
        this.labsPurchased = new ArrayList<Laboratory>();
        this.experiments = new ArrayList<Experiment>(); 
    }

    // Setters
    public void setBudget(int i) {
        this.budget = i;
    }
    public void increaseFinishedExperiment(Experiment experiment) {
        this.experiments.add(experiment);
    }

    public void increaseMoneyGained(int moneygained) {
        this.moneyGained += moneygained;
    }

    // Getters
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
